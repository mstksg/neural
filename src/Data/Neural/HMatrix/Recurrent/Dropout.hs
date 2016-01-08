{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Neural.HMatrix.Recurrent.Dropout where

import Control.Applicative
import Control.Monad.Random
import Data.Bool
import Data.Neural.HMatrix.Recurrent
import Data.Neural.HMatrix.Recurrent.Train
import Control.Lens
import Data.Neural.Types                   (KnownNet, NeuralActs(..))
import GHC.TypeLits
import GHC.TypeLits.List
import Numeric.AD.Rank1.Forward
import Numeric.LinearAlgebra.Static

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
    -> Double
    -> Double
    -> Double
    -> R o
    -> [R i]
    -> Network i hs o
    -> m (Network i hs o)
trainSeriesDO na doRate step stepS targ inps0 n0 =
    genNetMask doRate <&> \nm ->
      let n0M              = applyMask nm n0
          (ns0M, nu0M)     = toNetworkU n0M
          (dsM, nuShiftsM) = bptt na step targ inps0 ns0M nu0M
          (ns0, nu0)       = toNetworkU n0
      in  trainStates stepS (nu0 - nuShiftsM) ns0 dsM
{-# INLINE trainSeriesDO #-}

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
          _ -> error "impossible!"
{-# INLINE applyMask #-}

genNetMask
    :: forall i hs o m. (KnownNet i hs o, MonadRandom m)
    => Double
    -> m (NetMask i hs o)
genNetMask doRate = go natsList
  where
    go :: forall j js. KnownNat j => NatList js -> m (NetMask j js o)
    go nl = case nl of
              ØNL       -> return MaskOL
              _ :<# nl' -> liftA2 MaskIL randomMask (go nl')
    randomMask :: forall n. KnownNat n => m (R n)
    randomMask = dvmap (bool 0 1 . (doRate <)) . flip randomVector Uniform
             <$> getRandom
{-# INLINE genNetMask #-}

-- TODO: LITERALLY WRONG!!!!
compensateDO :: KnownNet i hs o => Double -> Network i hs o -> Network i hs o
compensateDO d n =
    case n of
      NetOL _ -> n
      NetIL (RLayer b wI wS s) n' ->
        NetIL (RLayer b wI (konst d * wS) s)
        (zipNet (*) (*) (konstNet d) n')
{-# INLINE compensateDO #-}
