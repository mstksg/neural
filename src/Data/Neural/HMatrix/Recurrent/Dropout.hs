{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Neural.HMatrix.Recurrent.Dropout
  ( trainSeriesDO
  , trainSeriesDOMWC
  , compensateDO
  )
  where

import Control.Applicative
import Control.Lens
import Control.Monad.Primitive
import Control.Monad.Random hiding         (uniform)
import Data.Bool
import Data.Neural.HMatrix.Recurrent
import Data.Neural.HMatrix.Recurrent.Train
import Data.Neural.HMatrix.FLayer
import Data.Neural.Types                   (KnownNet, NeuralActs(..))
import GHC.TypeLits
import GHC.TypeLits.List
import Numeric.AD.Rank1.Forward
import Numeric.LinearAlgebra.Static
import System.Random.MWC

-- should store `diag m` instead of `m`?
data NetMask :: Nat -> [Nat] -> Nat -> * where
    MaskOL :: NetMask i '[] o
    MaskIL :: (KnownNat j, KnownNats js)
           => !(R j) -> !(NetMask j js o) -> NetMask i (j ': js) o

infixr 5 `MaskIL`

deriving instance Show (NetMask i hs o)

trainSeriesDO
    :: forall i hs o m f. (KnownNet i hs o, MonadRandom m, Foldable f)
    => NeuralActs (Forward Double)
    -> Double   -- ^ Dropout rate
    -> Double   -- ^ Step size (weights)
    -> Double   -- ^ Step size (state)
    -> R o      -- ^ Target
    -> f (R i)  -- ^ Inputs
    -> Network i hs o
    -> m (Network i hs o)
trainSeriesDO na doRate step stepS targ inps0 n0 =
    genNetMask doRate <&> \nm ->
      let n0M              = applyMask nm n0
          (ns0M, nu0M)     = toNetworkU n0M
          (dsM, nuShiftsM) = bptt na step targ inps0 ns0M nu0M
          (ns0, nu0)       = toNetworkU n0
      in  trainStates stepS (nu0 - applyMaskU nm nuShiftsM) ns0 (applyMaskD nm dsM)
{-# INLINE trainSeriesDO #-}

trainSeriesDOMWC
    :: forall i hs o m f. (KnownNet i hs o, PrimMonad m, Foldable f)
    => NeuralActs (Forward Double)
    -> Double   -- ^ Dropout rate
    -> Double   -- ^ Step size (weights)
    -> Double   -- ^ Step size (state)
    -> R o      -- ^ Target
    -> f (R i)  -- ^ Inputs
    -> Network i hs o
    -> Gen (PrimState m)
    -> m (Network i hs o)
trainSeriesDOMWC na doRate step stepS targ inps0 n0 g =
    genNetMaskMWC doRate g <&> \nm ->
      let n0M              = applyMask nm n0
          (ns0M, nu0M)     = toNetworkU n0M
          (dsM, nuShiftsM) = bptt na step targ inps0 ns0M nu0M
          (ns0, nu0)       = toNetworkU n0
      in  trainStates stepS (nu0 - applyMaskU nm nuShiftsM) ns0 (applyMaskD nm dsM)
{-# INLINE trainSeriesDOMWC #-}

applyMask
    :: KnownNet i hs o
    => NetMask i hs o
    -> Network i hs o
    -> Network i hs o
applyMask = \case
    MaskOL -> id
    MaskIL m nm -> \case
      NetIL (RLayer b wI wS s) nn ->
        let mM = diag m
            nnMasked = case applyMask nm nn of
                         NetOL (FLayer b' w') ->
                           NetOL (FLayer b' (w' <> mM))
                         NetIL (RLayer b' wI' wS' s') nn' ->
                           NetIL (RLayer b' (wI' <> mM) wS' s') nn'
        in  NetIL (RLayer (m * b) (mM <> wI) (mM <> wS <> mM) (m * s))
                  nnMasked
{-# INLINE applyMask #-}

applyMaskU
    :: KnownNet i hs o
    => NetMask i hs o
    -> NetworkU i hs o
    -> NetworkU i hs o
applyMaskU = \case
    MaskOL -> id
    MaskIL m nm -> \case
      NetUIL (RLayerU b wI wS) nn ->
        let mM = diag m
            nnMasked = case applyMaskU nm nn of
                         NetUOL (FLayer b' w') ->
                           NetUOL (FLayer b' (w' <> mM))
                         NetUIL (RLayerU b' wI' wS') nn' ->
                           NetUIL (RLayerU b' (wI' <> mM) wS') nn'
        in  NetUIL (RLayerU (m * b) (mM <> wI) (mM <> wS <> mM))
                   nnMasked
{-# INLINE applyMaskU #-}

applyMaskD
    :: forall i hs o. KnownNet i hs o
    => NetMask i hs o
    -> Deltas i hs o
    -> Deltas i hs o
applyMaskD = \case
    MaskOL -> id
    MaskIL m nm -> \case
      DeltasIL dI dO (dlt :: Deltas h hs' o) ->
        let dltMasked :: Deltas h hs' o
            dltMasked = case applyMaskD nm dlt of
                          DeltasOL dI'          -> DeltasOL (m * dI')
                          DeltasIL dI' dO' dlt' -> DeltasIL (m * dI') dO' dlt'
        in  DeltasIL dI (m * dO) dltMasked
{-# INLINE applyMaskD #-}

genNetMask
    :: forall i hs o m. (KnownNet i hs o, MonadRandom m)
    => Double
    -> m (NetMask i hs o)
genNetMask doRate = go natsList
  where
    go :: forall j js. NatList js -> m (NetMask j js o)
    go nl = case nl of
              ØNL       -> return MaskOL
              _ :<# nl' -> liftA2 MaskIL randomMask (go nl')
    randomMask :: forall n. KnownNat n => m (R n)
    randomMask = dvmap (bool 0 1 . (doRate <)) . flip randomVector Uniform
             <$> getRandom
{-# INLINE genNetMask #-}

genNetMaskMWC
    :: forall i hs o m. (KnownNet i hs o, PrimMonad m)
    => Double
    -> Gen (PrimState m)
    -> m (NetMask i hs o)
genNetMaskMWC doRate g = go natsList
  where
    go :: forall j js. NatList js -> m (NetMask j js o)
    go nl = case nl of
              ØNL       -> return MaskOL
              _ :<# nl' -> liftA2 MaskIL randomMask (go nl')
    randomMask :: forall n. KnownNat n => m (R n)
    randomMask = dvmap (bool 0 1 . (doRate <)) . flip randomVector Uniform
             <$> uniform g
{-# INLINE genNetMaskMWC #-}

compensateDO
    :: forall i hs o. KnownNet i hs o
    => Double
    -> Network i hs o
    -> Network i hs o
compensateDO d = \case
    NetOL w   -> NetOL w
    NetIL l n -> NetIL l (go n)
  where
    go  :: forall h hs'. KnownNat h
        => Network h hs' o
        -> Network h hs' o
    go = \case NetOL w   -> NetOL (compFLayer w)
               NetIL w n -> NetIL (compRLayer w) (go n)
    compFLayer
        :: forall i' o'. (KnownNat i', KnownNat o')
        => FLayer i' o'
        -> FLayer i' o'
    compFLayer (FLayer b w) =
        FLayer b (konst d' * w)
    compRLayer
        :: forall i' o'. (KnownNat i', KnownNat o')
        => RLayer i' o'
        -> RLayer i' o'
    compRLayer (RLayer b wI wS s) =
        RLayer b (konst d' * wI) (konst d' * wS) s
    d' = 1 / (1 - d)
{-# INLINE compensateDO #-}

