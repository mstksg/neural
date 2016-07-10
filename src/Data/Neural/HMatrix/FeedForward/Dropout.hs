{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Neural.HMatrix.FeedForward.Dropout where

import           Control.Lens
import           Control.Monad.Random
import           Data.Bool
import           Data.Neural.HMatrix.FLayer
import           Data.Neural.HMatrix.FeedForward
import           Data.Neural.Types               (NeuralActs(..))
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           Numeric.AD.Rank1.Forward
import           Numeric.LinearAlgebra.Static

data NetMask :: Nat -> [Nat] -> Nat -> * where
    MaskOL :: NetMask i '[] o
    MaskIL :: KnownNat j
           => !(R j)
           -> !(NetMask j js o)
           -> NetMask i (j ': js) o

infixr 5 `MaskIL`

trainSampleDO
    :: forall i hs o m. (KnownNat i, SingI hs, KnownNat o, MonadRandom m)
    => NeuralActs (Forward Double)
    -> Double           -- ^ dropout rate (how much to DROP)
    -> Double           -- ^ learning rate
    -> R i              -- ^ input vector
    -> R o              -- ^ target vector
    -> Network i hs o   -- ^ network to train
    -> m (Network i hs o)
trainSampleDO na doRate step x0 target net0 =
    genNetMask doRate <&> \nm ->
      let masked :: Network i hs o
          masked = applyMask nm net0
          shift  :: Network i hs o
          shift  = trainStep na step x0 target masked
      in  net0 - applyMask nm shift
{-# INLINE trainSampleDO #-}

trainStep
    :: forall i hs o. (KnownNat i, KnownNat o)
    => NeuralActs (Forward Double)
    -> Double           -- ^ learning rate
    -> R i              -- ^ input vector
    -> R o              -- ^ target vector
    -> Network i hs o   -- ^ network to train
    -> Network i hs o
trainStep (NA f g) rate x0 target = fst . go x0
  where
    NA f_ g_ = NA (fst . diff' f) (fst . diff' g)
    go  :: forall j js. KnownNat j
        => R j              -- ^ input vector
        -> Network j js o   -- ^ network to train
        -> (Network j js o, R j)
    go !x = \case
      NetOL w@(FLayer _ wN) ->
        let y    = runFLayer w x
            o    = dvmap g_ y
            dEdy = dvmap (diff g) y * (o - target)
            wB'  = konst rate * dEdy
            wN'  = konst rate * (dEdy `outer` x)
            w'   = FLayer wB' wN'
            dWs  = tr wN #> dEdy
        in  (NetOL w', dWs)
      NetIL w@(FLayer _ wN) n ->
        let y          = runFLayer w x
            o          = dvmap f_ y
            (n', dWs') = go o n
            dEdy       = dvmap (diff f) y * dWs'
            wB'        = konst rate * dEdy
            wN'        = konst rate * (dEdy `outer` x)
            w'         = FLayer wB' wN'
            dWs        = tr wN #> dEdy
        in  (NetIL w' n', dWs)


applyMask
    :: (KnownNat i, KnownNat o)
    => NetMask i hs o
    -> Network i hs o
    -> Network i hs o
applyMask =
    \case MaskOL      -> id
          MaskIL m nm ->
            \case NetIL (FLayer b w) n ->
                    let mM = diag m
                        n' = case applyMask nm n of
                               NetOL (FLayer b' w')     ->
                                 NetOL (FLayer b' (w' <> mM))
                               NetIL (FLayer b' w') n'' ->
                                 NetIL (FLayer b' (w' <> mM)) n''
                    in  NetIL (FLayer (m * b) (mM <> w)) n'
{-# INLINE applyMask #-}

genNetMask
    :: forall i hs o m. (KnownNet i hs o, MonadRandom m)
    => Double           -- ^ dropout rate (how much to DROP)
    -> m (NetMask i hs o)
genNetMask doRate = go sing
  where
    go :: forall j js. Sing js -> m (NetMask j js o)
    go = \case SNil            -> return MaskOL
               SNat `SCons` ss -> MaskIL <$> randomMask <*> go ss
    randomMask :: forall n. KnownNat n => m (R n)
    randomMask = dvmap (bool 0 1 . (doRate <)) . flip randomVector Uniform
             <$> getRandom
{-# INLINE genNetMask #-}


compensateDO
    :: forall i hs o. KnownNat o
    => Double       -- ^ how much was DROPPED
    -> Network i hs o
    -> Network i hs o
compensateDO d = \case
    NetOL w   -> NetOL w
    NetIL w n -> NetIL w (go n)
  where
    go :: forall j js. KnownNat j => Network j js o -> Network j js o
    go = \case
      NetOL w   -> NetOL (compLayer w)
      NetIL w n -> NetIL (compLayer w) (go n)
    compLayer :: forall i' o'. (KnownNat i', KnownNat o') => FLayer i' o' -> FLayer i' o'
    compLayer = \case
        FLayer b w -> FLayer (konst d' * b) (konst d' * w)
    d' = 1 / (1 - d)
{-# INLINE compensateDO #-}
