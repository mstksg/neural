{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
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
    :: forall i hs o m. (KnownNet i hs o, MonadRandom m)
    => NeuralActs (Forward Double)
    -> Double   -- ^ Dropout rate
    -> Double   -- ^ Step size (weights)
    -> Double   -- ^ Step size (state)
    -> R o      -- ^ Target
    -> [R i]    -- ^ Inputs
    -> Network i hs o
    -> m (Network i hs o)
trainSeriesDO na doRate step stepS targ inps0 n0 =
    genNetMask doRate <&> \nm ->
      let n0M              = applyMask nm n0
          (ns0M, nu0M)     = toNetworkU n0M
          (dsM, nuShiftsM) = bptt na step targ inps0 ns0M nu0M
          (ns0, nu0)       = toNetworkU n0
      -- TODO: wait, should this mask changes too
      in  trainStates stepS (nu0 - nuShiftsM) ns0 dsM
{-# INLINE trainSeriesDO #-}

trainSeriesDOMWC
    :: forall i hs o m. (KnownNet i hs o, PrimMonad m)
    => NeuralActs (Forward Double)
    -> Double   -- ^ Dropout rate
    -> Double   -- ^ Step size (weights)
    -> Double   -- ^ Step size (state)
    -> R o      -- ^ Target
    -> [R i]    -- ^ Inputs
    -> Network i hs o
    -> Gen (PrimState m)
    -> m (Network i hs o)
trainSeriesDOMWC na doRate step stepS targ inps0 n0 g =
    genNetMaskMWC doRate g <&> \nm ->
      let n0M              = applyMask nm n0
          (ns0M, nu0M)     = toNetworkU n0M
          (dsM, nuShiftsM) = bptt na step targ inps0 ns0M nu0M
          (ns0, nu0)       = toNetworkU n0
      in  trainStates stepS (nu0 - nuShiftsM) ns0 dsM
{-# INLINE trainSeriesDOMWC #-}

applyMask
    :: KnownNet i hs o
    => NetMask i hs o
    -> Network i hs o
    -> Network i hs o
applyMask nm nn =
    case nn of
      NetOL _ -> nn
      NetIL (RLayer b wI wS s) nn' ->
        case nm of
          MaskIL m nm' ->
            let mM = diag m
                nnMasked = case applyMask nm' nn' of
                             NetOL (FLayer b' w')              ->
                               NetOL (FLayer b' (w' <> mM))
                             NetIL (RLayer b' wI' wS' s') nnM' ->
                               NetIL (RLayer b' (wI' <> mM) wS' s') nnM'
            in  NetIL (RLayer (m * b) (mM <> wI) (mM <> wS <> mM) (m * s))
                      nnMasked
{-# INLINE applyMask #-}

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

-- TODO: is this seriously wrong or what
compensateDO :: KnownNet i hs o => Double -> Network i hs o -> Network i hs o
compensateDO d n =
    case n of
      NetOL _ -> n
      NetIL (RLayer b wI wS s) n' ->
        NetIL (RLayer b wI (konst (1 - d) * wS) s)
              (zipNet (*) (*) (konstNet (1 - d)) n')
{-# INLINE compensateDO #-}
