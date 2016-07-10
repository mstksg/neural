{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Neural.HMatrix.FeedForward where

import           Control.DeepSeq
import           Control.Monad.Primitive
import           Control.Monad.Random
import           Data.MonoTraversable
import           Data.Neural.HMatrix.FLayer
import           Data.Neural.Types            (NeuralActs(..))
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           Numeric.AD.Rank1.Forward
import           Numeric.LinearAlgebra.Static
import           System.Random.MWC
import qualified Data.Binary                  as B

type KnownNet i hs o = (KnownNat i, SingI hs, KnownNat o)

data Network :: Nat -> [Nat] -> Nat -> * where
    NetOL :: !(FLayer i o) -> Network i '[] o
    NetIL :: KnownNat j
          => !(FLayer i j) -> !(Network j hs o) -> Network i (j ': hs) o

infixr 5 `NetIL`

data SomeNet :: * where
    SomeNet :: (KnownNat i, KnownNat o) => Network i hs o -> SomeNet

data OpaqueNet :: Nat -> Nat -> * where
    OpaqueNet :: Network i hs o -> OpaqueNet i o

-- deriving instance KnownNet i hs o => Show (Network i hs o)
-- deriving instance Show SomeNet
-- deriving instance (KnownNat i, KnownNat o) => Show (OpaqueNet i o)

zipNet
    :: forall i hs o. (KnownNat i, KnownNat o)
    => (forall i' o'. (KnownNat i', KnownNat o') => FLayer i' o' -> FLayer i' o' -> FLayer i' o')
    -> Network i hs o
    -> Network i hs o
    -> Network i hs o
zipNet f = go
  where
    go  :: forall j js. KnownNat j
        => Network j js o
        -> Network j js o
        -> Network j js o
    go = \case
      NetOL l1 -> \case
        NetOL l2 -> NetOL (f l1 l2)
      NetIL l1 n1 -> \case
        NetIL l2 n2 -> NetIL (f l1 l2) (go n1 n2)
{-# INLINE zipNet #-}

mapNet
    :: forall i hs o. (KnownNat i, KnownNat o)
    => (forall i' o'. (KnownNat i', KnownNat o') => FLayer i' o' -> FLayer i' o')
    -> Network i hs o
    -> Network i hs o
mapNet f = go
  where
    go  :: forall j js. KnownNat j
        => Network j js o
        -> Network j js o
    go = \case
      NetOL l   -> NetOL (f l)
      NetIL l n -> NetIL (f l) (go n)
{-# INLINE mapNet #-}


instance KnownNet i hs o => Num (Network i hs o) where
    (+)           = zipNet (+)
    {-# INLINE (+) #-}
    (-)           = zipNet (-)
    {-# INLINE (-) #-}
    (*)           = zipNet (*)
    {-# INLINE (*) #-}
    negate        = mapNet negate
    {-# INLINE negate #-}
    abs           = mapNet abs
    {-# INLINE abs #-}
    signum        = mapNet signum
    {-# INLINE signum #-}
    fromInteger x = pureNet (fromInteger x)
    {-# INLINE fromInteger #-}


type instance Element (Network i hs o) = Double

instance NFData (Network i hs o) where
    rnf (NetOL (force -> !_)) = ()
    rnf (NetIL (force -> !_) (force -> !_)) = ()

putNet :: (KnownNat i, KnownNat o) => Network i hs o -> B.Put
putNet = \case NetOL w   -> B.put w
               NetIL w n -> B.put w *> putNet n

getNet :: (KnownNat i, KnownNat o) => Sing hs -> B.Get (Network i hs o)
getNet = \case SNil           -> NetOL <$> B.get
               SNat `SCons` s -> NetIL <$> B.get <*> getNet s

instance KnownNet i hs o => B.Binary (Network i hs o) where
    put = putNet
    get = getNet sing

instance (KnownNat i, KnownNat o) => B.Binary (OpaqueNet i o) where
    put = \case OpaqueNet n -> do
                  B.put (fromSing (hiddenSing n))
                  putNet n
    get = do
      hs <- B.get
      withSomeSing hs (fmap OpaqueNet . getNet)

instance NFData (OpaqueNet i o) where
    rnf = \case OpaqueNet (force -> !_) -> ()

instance B.Binary SomeNet where
    put = \case SomeNet (n :: Network i hs o) -> do
                  B.put (natVal (Proxy @i))
                  B.put (fromSing (hiddenSing n))
                  B.put (natVal (Proxy @o))
                  putNet n
    get = do
      i  <- B.get
      hs <- B.get
      o  <- B.get
      withSomeSing i    $ \(SNat :: Sing (i  :: Nat  )) ->
        withSomeSing hs $ \(hs'  :: Sing (hs :: [Nat])) ->
        withSomeSing o  $ \(SNat :: Sing (o  :: Nat  )) -> do
          n <- getNet hs'
          return $ SomeNet (n :: Network i hs o)

instance NFData SomeNet where
    rnf = \case SomeNet (force -> !_) -> ()


hiddenSing :: forall i hs o. Network i hs o -> Sing hs
hiddenSing = \case NetOL _                   -> SNil
                   NetIL (_ :: FLayer i h) n -> SNat @h `SCons` hiddenSing n

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
{-# INLINE pureNet #-}

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
{-# INLINE trainSample #-}

traverseOpaqueNet
    :: Applicative f
    => (forall hs. Network i hs o -> f (Network i' hs o'))
    -> OpaqueNet i o
    -> f (OpaqueNet i' o')
traverseOpaqueNet f = \case OpaqueNet net -> OpaqueNet <$> f net
{-# INLINE traverseOpaqueNet #-}

mapOpaqueNet
    :: (forall hs. Network i hs o -> Network i' hs o')
    -> OpaqueNet i o
    -> OpaqueNet i' o'
mapOpaqueNet f = \case OpaqueNet net -> OpaqueNet (f net)
{-# INLINE mapOpaqueNet #-}

randomNet
    :: forall m i hs o. (MonadRandom m, KnownNet i hs o)
    => (Double, Double)
    -> m (Network i hs o)
randomNet = randomNetSing sing

randomNetSing
    :: forall i hs o m. (KnownNat i, KnownNat o, MonadRandom m)
    => Sing hs
    -> (Double, Double)
    -> m (Network i hs o)
randomNetSing s r = go s
  where
    go  :: forall j js. KnownNat j
        => Sing js
        -> m (Network j js o)
    go = \case SNil            -> NetOL <$> randomFLayer r
               SNat `SCons` s' -> NetIL <$> randomFLayer r <*> go s'

randomNetMWC
    :: forall m i hs o. (PrimMonad m, KnownNet i hs o)
    => (Double, Double)
    -> Gen (PrimState m)
    -> m (Network i hs o)
randomNetMWC = randomNetMWCSing sing

randomNetMWCSing
    :: forall m i hs o. (PrimMonad m, KnownNat i, KnownNat o)
    => Sing hs
    -> (Double, Double)
    -> Gen (PrimState m)
    -> m (Network i hs o)
randomNetMWCSing s r g = go s
  where
    go  :: forall j js. KnownNat j
        => Sing js
        -> m (Network j js o)
    go nl = case nl of
              SNil             -> NetOL <$> randomFLayerMWC r g
              SNat `SCons` nl' -> NetIL <$> randomFLayerMWC r g <*> go nl'

randomONet
    :: (KnownNat i, KnownNat o, MonadRandom m)
    => [Integer]
    -> (Double, Double)
    -> m (OpaqueNet i o)
randomONet hs r = withSomeSing hs $ \hsS ->
                    OpaqueNet <$> randomNetSing hsS r

randomONetMWC
    :: (KnownNat i, KnownNat o, PrimMonad m)
    => [Integer]
    -> (Double, Double)
    -> Gen (PrimState m)
    -> m (OpaqueNet i o)
randomONetMWC hs r g = withSomeSing hs $ \hsS ->
                         OpaqueNet <$> randomNetMWCSing hsS r g

runOpaqueNet
    :: forall i o. (KnownNat i, KnownNat o)
    => NeuralActs Double
    -> OpaqueNet i o
    -> R i
    -> R o
runOpaqueNet na oN x = case oN of
                         OpaqueNet net -> runNetwork na net x
{-# INLINE runOpaqueNet #-}
