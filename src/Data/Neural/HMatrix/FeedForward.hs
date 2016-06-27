{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Neural.HMatrix.FeedForward where

-- import GHC.TypeLits
-- import GHC.TypeLits.List
import Control.Monad.Primitive
import Control.Monad.Random
import Data.MonoTraversable
import Data.Neural.HMatrix.Types
import Data.Neural.Types            (NeuralActs(..))
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Numeric.AD.Rank1.Forward
import Numeric.LinearAlgebra.Static
import System.Random.MWC

type KnownNet i hs o = (KnownNat i, SingI hs, KnownNat o)

data Network :: Nat -> [Nat] -> Nat -> * where
    NetOL :: !(FLayer i o) -> Network i '[] o
    NetIL :: KnownNat j
          => !(FLayer i j) -> !(Network j hs o) -> Network i (j ': hs) o

infixr 5 `NetIL`

data SomeNet :: * where
    SomeNet :: KnownNet i hs o => Network i hs o -> SomeNet

data OpaqueNet :: Nat -> Nat -> * where
    OpaqueNet :: SingI hs => Network i hs o -> OpaqueNet i o

-- deriving instance KnownNet i hs o => Show (Network i hs o)
-- deriving instance Show SomeNet
-- deriving instance (KnownNat i, KnownNat o) => Show (OpaqueNet i o)

type instance Element (Network i hs o) = Double

pureNet
    :: forall i hs o. KnownNet i hs o
    => (forall j k. (KnownNat j, KnownNat k) => FLayer j k)
    -> Network i hs o
pureNet l = go sing
  where
    go :: forall j js. KnownNat j => Sing js -> Network j js o
    go nl = case nl of
              SNil             -> NetOL l
              SNat `SCons` nl' -> l `NetIL` go nl'

runNetwork
    :: forall i hs o. (KnownNat i, KnownNat o)
    => NeuralActs Double
    -> Network i hs o
    -> R i
    -> R o
runNetwork (NA f g) = go
  where
    go :: forall i' hs'. KnownNat i'
       => Network i' hs' o
       -> R i'
       -> R o
    go n v = case n of
               NetOL l    -> dvmap g (runFLayer l v)
               NetIL l nI -> let v'  = runFLayer l v
                                 v'' = go nI (dvmap f v')
                             in  v''
{-# INLINE runNetwork #-}

trainSample
    :: forall i hs o. (KnownNat i, KnownNat o)
    => NeuralActs (Forward Double)
    -> Double           -- ^ learning rate
    -> R i              -- ^ input vector
    -> R o              -- ^ target vector
    -> Network i hs o   -- ^ network to train
    -> Network i hs o
trainSample (NA f g) rate x0 target = fst . go x0
  where
    NA f_ g_ = NA (fst . diff' f) (fst . diff' g)
    go  :: forall j js. KnownNat j
        => R j              -- ^ input vector
        -> Network j js o   -- ^ network to train
        -> (Network j js o, R j)
    go !x (NetOL w@(FLayer wB wN))
        = let y    = runFLayer w x
              o    = dvmap g_ y
              dEdy = dvmap (diff g) y * (o - target)
              wB'  = wB - konst rate * dEdy
              wN'  = wN - konst rate * (dEdy `outer` x)
              w'   = FLayer wB' wN'
              dWs  = tr wN #> dEdy
          in  (NetOL w', dWs)
    go !x (NetIL w@(FLayer wB wN) n)
        = let y          = runFLayer w x
              o          = dvmap f_ y
              (n', dWs') = go o n
              dEdy       = dvmap (diff f) y * dWs'
              wB'  = wB - konst rate * dEdy
              wN'  = wN - konst rate * (dEdy `outer` x)
              w'   = FLayer wB' wN'
              dWs  = tr wN #> dEdy
          in  (NetIL w' n', dWs)

randomNet
    :: forall m i hs o. (MonadRandom m, KnownNet i hs o)
    => (Double, Double)
    -> m (Network i hs o)
randomNet r = go sing
  where
    go :: forall j js. KnownNat j
       => Sing js
       -> m (Network j js o)
    go nl = case nl of
              SNil             -> NetOL <$> randomFLayer r
              SNat `SCons` nl' -> NetIL <$> randomFLayer r <*> go nl'

randomNetMWC
    :: forall m i hs o. (PrimMonad m, KnownNet i hs o)
    => (Double, Double)
    -> Gen (PrimState m)
    -> m (Network i hs o)
randomNetMWC r g = go sing
  where
    go :: forall j js. KnownNat j
       => Sing js
       -> m (Network j js o)
    go nl = case nl of
              SNil             -> NetOL <$> randomFLayerMWC r g
              SNat `SCons` nl' -> NetIL <$> randomFLayerMWC r g <*> go nl'

