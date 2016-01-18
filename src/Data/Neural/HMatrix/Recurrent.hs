{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Neural.HMatrix.Recurrent where

-- import Data.Containers
import Control.DeepSeq
import Control.Monad.Random            as R
import Data.Foldable
import Control.Monad.State
import Data.MonoTraversable
import Data.Neural.Types               (KnownNet, NeuralActs(..))
import Data.Proxy
import Data.Reflection
import GHC.Generics                    (Generic)
import GHC.TypeLits
import GHC.TypeLits.List
import Numeric.LinearAlgebra.Static
import qualified Data.Binary           as B
import qualified Data.Neural.Recurrent as N
import qualified Data.Neural.Types     as N
import qualified Data.Vector           as V
import qualified Data.Vector.Generic   as VG
import qualified Linear.V              as L
import qualified Numeric.LinearAlgebra as H

data FLayer :: Nat -> Nat -> * where
    FLayer :: { fLayerBiases  :: !(R o)
              , fLayerWeights :: !(L o i)
              } -> FLayer i o
  deriving (Show, Generic)


data RLayer :: Nat -> Nat -> * where
    RLayer :: { rLayerBiases   :: !(R o)
              , rLayerIWeights :: !(L o i)
              , rLayerSWeights :: !(L o o)
              , rLayerState    :: !(R o)
              } -> RLayer i o
  deriving (Show, Generic)

data Network :: Nat -> [Nat] -> Nat -> * where
    NetOL :: !(FLayer i o) -> Network i '[] o
    NetIL :: (KnownNat j, KnownNats hs)
          => !(RLayer i j) -> !(Network j hs o) -> Network i (j ': hs) o

infixr 5 `NetIL`

data NetActs :: Nat -> [Nat] -> Nat -> * where
    NetAOL :: !(R o) -> NetActs i hs o
    NetAIL :: (KnownNat j, KnownNats hs) => !(R j) -> !(NetActs j hs o) -> NetActs i (j ': js) o

infixr 5 `NetAIL`

data SomeNet :: * where
    SomeNet :: KnownNet i hs o => Network i hs o -> SomeNet

data OpaqueNet :: Nat -> Nat -> * where
    OpaqueNet :: KnownNats hs => Network i hs o -> OpaqueNet i o

data SomeFLayer :: * where
    SomeFLayer :: (KnownNat i, KnownNat o) => FLayer i o -> SomeFLayer

deriving instance KnownNet i hs o => Show (Network i hs o)
deriving instance KnownNet i hs o => Show (NetActs i hs o)
deriving instance Show SomeNet
deriving instance (KnownNat i, KnownNat o) => Show (OpaqueNet i o)
deriving instance Show SomeFLayer

type instance Element (FLayer i o) = Double

instance (KnownNat i, KnownNat o) => MonoFunctor (FLayer i o) where
    omap f (FLayer b w) = FLayer (dvmap f b) (dmmap f w)

-- instance MonoZip (FLayer i o) where
--     ozipWith f (FLayer b1 w1) (FLayer b2 w2) = FLayer ()

konstFLayer :: (KnownNat i, KnownNat o)
            => Double
            -> FLayer i o
konstFLayer = FLayer <$> konst <*> konst

instance (KnownNat i, KnownNat o) => Num (FLayer i o) where
    FLayer b1 w1 + FLayer b2 w2 = FLayer (b1 + b2) (w1 + w2)
    FLayer b1 w1 * FLayer b2 w2 = FLayer (b1 * b2) (w1 * w2)
    FLayer b1 w1 - FLayer b2 w2 = FLayer (b1 - b2) (w1 - w2)
    abs (FLayer b w) = FLayer (abs b) (abs w)
    negate (FLayer b w) = FLayer (negate b) (negate w)
    signum (FLayer b w) = FLayer (signum b) (signum w)
    fromInteger = FLayer <$> fromInteger <*> fromInteger

konstRLayer :: (KnownNat i, KnownNat o)
            => Double
            -> RLayer i o
konstRLayer = RLayer <$> konst <*> konst <*> konst <*> konst

instance (KnownNat i, KnownNat o) => Num (RLayer i o) where
    RLayer b1 wI1 wS1 s1 + RLayer b2 wI2 wS2 s2 = RLayer (b1  + b2)
                                                         (wI1 + wI2)
                                                         (wS1 + wS2)
                                                         (s1  + s2)
    RLayer b1 wI1 wS1 s1 * RLayer b2 wI2 wS2 s2 = RLayer (b1  * b2)
                                                         (wI1 * wI2)
                                                         (wS1 * wS2)
                                                         (s1  * s2)
    RLayer b1 wI1 wS1 s1 - RLayer b2 wI2 wS2 s2 = RLayer (b1  - b2)
                                                         (wI1 - wI2)
                                                         (wS1 - wS2)
                                                         (s1  - s2)
    abs    (RLayer b wI wS s) = RLayer (abs b) (abs wI) (abs wS) (abs s)
    negate (RLayer b wI wS s) = RLayer (negate b) (negate wI) (negate wS) (negate s)
    signum (RLayer b wI wS s) = RLayer (signum b) (signum wI) (signum wS) (signum s)
    fromInteger = RLayer <$> fromInteger
                         <*> fromInteger
                         <*> fromInteger
                         <*> fromInteger


pureNet :: forall i hs o. KnownNet i hs o
        => (forall j k. (KnownNat j, KnownNat k) => FLayer j k)
        -> (forall j k. (KnownNat j, KnownNat k) => RLayer j k)
        -> Network i hs o
pureNet lf lr = go natsList
  where
    go :: forall j js. KnownNat j => NatList js -> Network j js o
    go nl = case nl of
           ØNL       -> NetOL lf
           _ :<# nl' -> lr `NetIL` go nl'

konstNet :: KnownNet i hs o => Double -> Network i hs o
konstNet x = pureNet (konstFLayer x) (konstRLayer x)

zipNet
    :: forall i hs o. KnownNet i hs o
    => (forall j k. (KnownNat j, KnownNat k) => FLayer j k -> FLayer j k -> FLayer j k)
    -> (forall j k. (KnownNat j, KnownNat k) => RLayer j k -> RLayer j k -> RLayer j k)
    -> Network i hs o -> Network i hs o
    -> Network i hs o
zipNet ff fr = go
  where
    go :: forall j js. KnownNet j js o => Network j js o -> Network j js o -> Network j js o
    go n1 n2 = case n1 of
                 NetOL l1 ->
                   case n2 of
                     NetOL l2 -> NetOL (ff l1 l2)
                     _        -> error "impossible"
                 NetIL l1 n1' ->
                   case n2 of
                     NetIL l2 n2' ->
                       NetIL (fr l1 l2) (go n1' n2')
                     _             ->
                       error "impossible"


-- TODO: can be done better now
randomVec :: forall g n. (RandomGen g, KnownNat n)
          => (Double, Double) -> g -> (R n, g)
randomVec r = runRand $
  fmap vector . replicateM (fromInteger (natVal (Proxy :: Proxy n))) $
    getRandomR r

randomMat :: forall g n m. (RandomGen g, KnownNat n, KnownNat m)
          => (Double, Double) -> g -> (L n m, g)
randomMat r = runRand $
    fmap matrix . replicateM (fromInteger s) $
      getRandomR r
  where
    s = natVal (Proxy :: Proxy n) * natVal (Proxy :: Proxy m)


instance (KnownNat i, KnownNat o) => Random (FLayer i o) where
    random = runRand $
        FLayer <$> liftRand (randomVec (-1, 1))
               <*> liftRand (randomMat (-1, 1))
    randomR  = error "FLayer i o (randomR): Unimplemented"

instance (KnownNat i, KnownNat o) => Random (RLayer i o) where
    random = runRand $
        RLayer <$> liftRand (randomVec (-1, 1))
               <*> liftRand (randomMat (-1, 1))
               <*> liftRand (randomMat (-1, 1))
               <*> liftRand (randomVec (-1, 1))
    randomR  = error "RLayer i o (randomR): Unimplemented"

instance KnownNet i hs o => Random (Network i hs o) where
    random :: forall g. RandomGen g => g -> (Network i hs o, g)
    random = runRand $ go natsList
      where
        go :: forall j js. KnownNat j
           => NatList js
           -> Rand g (Network j js o)
        go nl = case nl of
                  ØNL       -> NetOL <$> getRandom
                  _ :<# nl' -> NetIL <$> getRandom <*> go nl'
    randomR  = error "Network i hs o (randomR): Unimplemented"

instance NFData (FLayer i o)
instance NFData (RLayer i o)

instance NFData (Network i hs o) where
    rnf (NetOL (force -> !_)) = ()
    rnf (NetIL (force -> !_) (force -> !_)) = ()

instance NFData (NetActs i hs o) where
    rnf (NetAOL (force -> !_)) = ()
    rnf (NetAIL (force -> !_) (force -> !_)) = ()

instance (KnownNat i, KnownNat o) => B.Binary (FLayer i o) where
instance (KnownNat i, KnownNat o) => B.Binary (RLayer i o) where

instance (KnownNat n, KnownNat m) => B.Binary (L n m) where
    put l = B.put (unwrap l)
    get   = do
      m <- B.get
      case create m of
        Just l  -> return l
        Nothing -> fail "L: Improper stored matrix size"

instance (KnownNat n) => B.Binary (R n) where
    put l = B.put (unwrap l)
    get   = do
      v <- B.get
      case create v of
        Just l  -> return l
        Nothing -> fail "R: Improper stored vector size"

instance B.Binary SomeFLayer where
    put sl = case sl of
               SomeFLayer (l :: FLayer i o) -> do
                 B.put $ natVal (Proxy :: Proxy i)
                 B.put $ natVal (Proxy :: Proxy o)
                 B.put l
    get = do
      i <- B.get
      o <- B.get
      reifyNat i $ \(Proxy :: Proxy i) ->
        reifyNat o $ \(Proxy :: Proxy o) ->
          SomeFLayer <$> (B.get :: B.Get (FLayer i o))

instance KnownNet i hs o => B.Binary (Network i hs o) where
    put (NetOL l)    = B.put l
    put (NetIL l n') = B.put l *> B.put n'
    get = go natsList
      where
        go :: forall j js. KnownNat j
           => NatList js
           -> B.Get (Network j js o)
        go nl = case nl of
                  ØNL       -> NetOL <$> B.get
                  _ :<# nl' -> NetIL <$> B.get <*> go nl'

instance B.Binary SomeNet where
    put sn = case sn of
               SomeNet (n :: Network i hs o) -> do
                 B.put $ natVal (Proxy :: Proxy i)
                 B.put $ natVal (Proxy :: Proxy o)
                 B.put $ OpaqueNet n
    get = do
      i <- B.get
      o <- B.get
      reifyNat i $ \(Proxy :: Proxy i) ->
        reifyNat o $ \(Proxy :: Proxy o) -> do
          oqn <- B.get :: B.Get (OpaqueNet i o)
          return $ case oqn of
                     OpaqueNet n -> SomeNet n

instance (KnownNat i, KnownNat o) => B.Binary (OpaqueNet i o) where
    put oqn = case oqn of
                OpaqueNet n -> do
                  case n of
                    NetOL l -> do
                      B.put True
                      B.put l
                    NetIL (l :: RLayer i j) (n' :: Network j js o) -> do
                      B.put False
                      B.put $ natVal (Proxy :: Proxy j)
                      B.put l
                      B.put (OpaqueNet n')
    get = do
      isOL <- B.get
      if isOL
        then do
          OpaqueNet . NetOL <$> B.get
        else do
          j <- B.get
          reifyNat j $ \(Proxy :: Proxy j) -> do
            l   <- B.get :: B.Get (RLayer i j)
            nqo <- B.get :: B.Get (OpaqueNet j o)
            return $ case nqo of
              OpaqueNet n -> OpaqueNet $ l `NetIL` n

netActsOut :: NetActs i hs o -> R o
netActsOut n = case n of
                 NetAIL _ n' -> netActsOut n'
                 NetAOL l    -> l

runFLayer :: (KnownNat i, KnownNat o) => FLayer i o -> R i -> R o
runFLayer (FLayer b w) v = b + w #> v
{-# INLINE runFLayer #-}

runRLayer :: (KnownNat i, KnownNat o)
          => (Double -> Double)
          -> RLayer i o
          -> R i
          -> (R o, RLayer i o)
runRLayer f l@(RLayer b wI wS s) v = (v', l { rLayerState = dvmap f v' })
  where
    v'       = b + wI #> v + wS #> s
{-# INLINE runRLayer #-}

runNetwork :: forall i hs o. (KnownNat i, KnownNat o)
           => NeuralActs Double
           -> Network i hs o
           -> R i
           -> (R o, Network i hs o)
runNetwork (NA f g) = go
  where
    go :: forall i' hs'. KnownNat i'
       => Network i' hs' o
       -> R i'
       -> (R o, Network i' hs' o)
    go n v = case n of
               NetOL l    -> (dvmap g (runFLayer l v), n)
               NetIL l nI -> let (v' , l')  = runRLayer f l v
                                 (v'', nI') = go nI (dvmap f v')
                             in  (v'', NetIL l' nI')
{-# INLINE runNetwork #-}

runNetwork_ :: forall i hs o. (KnownNat i, KnownNat o)
            => NeuralActs Double
            -> Network i hs o
            -> R i
            -> R o
runNetwork_ na n = fst . runNetwork na n
{-# INLINE runNetwork_ #-}

runNetworkS :: (KnownNat i, KnownNat o, MonadState (Network i hs o) m)
            => NeuralActs Double
            -> R i
            -> m (R o)
runNetworkS na v = state (\n -> runNetwork na n v)
{-# INLINE runNetworkS #-}

runNetworkActs :: forall i hs o. (KnownNat i, KnownNat o)
               => NeuralActs Double
               -> Network i hs o
               -> R i
               -> (NetActs i hs o, Network i hs o)
runNetworkActs (NA f g) = go
  where
    go :: forall i' hs'. KnownNat i'
       => Network i' hs' o
       -> R i'
       -> (NetActs i' hs' o, Network i' hs' o)
    go n v = case n of
               NetOL l    -> (NetAOL (dvmap g (runFLayer l v)), n)
               NetIL l nI -> let (v' , l') = runRLayer f l v
                                 vRes      = dvmap f v'
                                 (nA, nI') = go nI vRes
                             in  (NetAIL vRes nA, NetIL l' nI')
{-# INLINE runNetworkActs #-}

runNetworkActsS :: (KnownNat i, KnownNat o, MonadState (Network i hs o) m)
                => NeuralActs Double
                -> R i
                -> m (NetActs i hs o)
runNetworkActsS na v = state (\n -> runNetworkActs na n v)
{-# INLINE runNetworkActsS #-}

runNetStream :: (KnownNat i, KnownNat o)
             => NeuralActs Double
             -> Network i hs o
             -> [R i]
             -> ([R o], Network i hs o)
runNetStream na n vs = runState (mapM (runNetworkS na) vs) n
{-# INLINE runNetStream #-}

runNetStream_ :: forall i hs o. (KnownNat i, KnownNat o)
              => NeuralActs Double
              -> Network i hs o
              -> [R i]
              -> [R o]
runNetStream_ na = go
  where
    go :: Network i hs o -> [R i] -> [R o]
    go n (v:vs) = let (u, n') = runNetwork na n v
                  in  u `deepseq` n' `deepseq` u : go n' vs
    go _ []     = []
{-# INLINE runNetStream_ #-}

runNetStreamActs :: (KnownNat i, KnownNat o)
                 => NeuralActs Double
                 -> Network i hs o
                 -> [R i]
                 -> ([NetActs i hs o], Network i hs o)
runNetStreamActs na n vs = runState (mapM (runNetworkActsS na) vs) n
{-# INLINE runNetStreamActs #-}

runNetStreamActs_ :: forall i hs o. (KnownNat i, KnownNat o)
                  => NeuralActs Double
                  -> Network i hs o
                  -> [R i]
                  -> [NetActs i hs o]
runNetStreamActs_ na = go
  where
    go :: Network i hs o -> [R i] -> [NetActs i hs o]
    go n (v:vs) = let (u, n') = runNetworkActs na n v
                  in  u `deepseq` n' `deepseq` u : go n' vs
    go _ []     = []
{-# INLINE runNetStreamActs_ #-}

runNetFeedback :: forall i hs o. (KnownNat i, KnownNat o)
               => NeuralActs Double
               -> (R o -> R i)
               -> Network i hs o
               -> R i
               -> [(R o, Network i hs o)]
runNetFeedback na nxt = go
  where
    go :: Network i hs o -> R i -> [(R o, Network i hs o)]
    go n v = let res@(v', n') = runNetwork na n v
             in  res : go n' (nxt v')
{-# INLINE runNetFeedback #-}

runNetFeedback_ :: forall i hs o. (KnownNat i, KnownNat o)
                => NeuralActs Double
                -> (R o -> R i)
                -> Network i hs o
                -> R i
                -> [R o]
runNetFeedback_ na nxt = go
  where
    go :: Network i hs o -> R i -> [R o]
    go n v = let (v', n') = runNetwork na n v
             in  v' : go n' (nxt v')
{-# INLINE runNetFeedback_ #-}

runNetFeedbackM :: forall i hs o m. (KnownNat i, Monad m, KnownNat o)
                => NeuralActs Double
                -> (R o -> m (R i))
                -> Network i hs o
                -> Int
                -> R i
                -> m [(R o, Network i hs o)]
runNetFeedbackM na nxt = go
  where
    go :: Network i hs o -> Int -> R i -> m [(R o, Network i hs o)]
    go n i v | i <= 0    = return []
             | otherwise = do
                 let vn'@(v', n') = runNetwork na n v
                 vsns <- go n' (i - 1) =<< nxt v'
                 return $ vn' : vsns

runNetFeedbackM_ :: forall i hs o m. (KnownNat i, Monad m, KnownNat o)
                 => NeuralActs Double
                 -> (R o -> m (R i))
                 -> Network i hs o
                 -> Int
                 -> R i
                 -> m [R o]
runNetFeedbackM_ na nxt = go
  where
    go :: Network i hs o -> Int -> R i -> m [R o]
    go n i v | i <= 0    = return []
             | otherwise = do
                 let (v', n') = runNetwork na n v
                 vs <- go n' (i - 1) =<< nxt v'
                 return $ v' : vs

runNetActsFeedback
    :: forall i hs o. (KnownNat i, KnownNat o)
    => NeuralActs Double
    -> (R o -> R i)
    -> Network i hs o
    -> R i
    -> [(NetActs i hs o, Network i hs o)]
runNetActsFeedback na nxt = go
  where
    go :: Network i hs o -> R i -> [(NetActs i hs o, Network i hs o)]
    go n v = let res@(nacts, n') = runNetworkActs na n v
                 v' = netActsOut nacts
             in  res : go n' (nxt v')

runNetActsFeedback_
    :: forall i hs o. (KnownNat i, KnownNat o)
    => NeuralActs Double
    -> (R o -> R i)
    -> Network i hs o
    -> R i
    -> [NetActs i hs o]
runNetActsFeedback_ na nxt = go
  where
    go :: Network i hs o -> R i -> [NetActs i hs o]
    go n v = let (nacts, n') = runNetworkActs na n v
                 v' = netActsOut nacts
             in  nacts : go n' (nxt v')

runNetActsFeedbackM
    :: forall i hs o m. (KnownNat i, Monad m, KnownNat o)
    => NeuralActs Double
    -> (R o -> m (R i))
    -> Network i hs o
    -> Int
    -> R i
    -> m [(NetActs i hs o, Network i hs o)]
runNetActsFeedbackM na nxt = go
  where
    go :: Network i hs o -> Int -> R i -> m [(NetActs i hs o, Network i hs o)]
    go n i v | i <= 0    = return []
             | otherwise = do
                 let res@(nacts, n') = runNetworkActs na n v
                     v' = netActsOut nacts
                 vsns <- go n' (i - 1) =<< nxt v'
                 return $ res : vsns

runNetActsFeedbackM_
    :: forall i hs o m. (KnownNat i, Monad m, KnownNat o)
    => NeuralActs Double
    -> (R o -> m (R i))
    -> Network i hs o
    -> Int
    -> R i
    -> m [NetActs i hs o]
runNetActsFeedbackM_ na nxt = go
  where
    go :: Network i hs o -> Int -> R i -> m [NetActs i hs o]
    go n i v | i <= 0    = return []
             | otherwise = do
                 let (nacts, n') = runNetworkActs na n v
                     v' = netActsOut nacts
                 ns <- go n' (i - 1) =<< nxt v'
                 return $ nacts : ns


fLayerFromHMat :: (KnownNat i, KnownNat o) => FLayer i o ->  N.FLayer i o Double
fLayerFromHMat (FLayer b w) = N.FLayer . L.V . V.fromList $ zipWith N.Node bl wl
  where
    bl = H.toList (extract b)
    wl = map (L.V . VG.convert . extract) $ toRows w

rLayerFromHMat :: (KnownNat i, KnownNat o) => RLayer i o -> N.RLayer i o Double
rLayerFromHMat (RLayer b wI wS s) = N.RLayer (L.V . V.fromList $ zipWith3 N.RNode bl wIl wSl)
                                             (L.V sv)
  where
    bl = H.toList (extract b)
    sv = VG.convert (extract s)
    wIl = map (L.V . VG.convert . extract) $ toRows wI
    wSl = map (L.V . VG.convert . extract) $ toRows wS

networkFromHMat :: KnownNet i hs o => Network i hs o -> N.Network i hs o Double
networkFromHMat n = case n of
                      NetOL l    -> N.NetOL (fLayerFromHMat l)
                      NetIL l n' -> rLayerFromHMat l `N.NetIL` networkFromHMat n'

fLayerFromV :: (KnownNat i, KnownNat o) => N.FLayer i o Double -> FLayer i o
fLayerFromV (N.FLayer n) = FLayer b w
  where
    Just b = create . VG.convert . L.toVector $ N.nodeBias <$> n
    Just w = create . H.fromRows . toList $ VG.convert . L.toVector . N.nodeWeights <$> n

rLayerFromV :: (KnownNat i, KnownNat o) => N.RLayer i o Double -> RLayer i o
rLayerFromV (N.RLayer n s0) = RLayer b wI wS s
  where
    Just b = create . VG.convert . L.toVector $ N.rNodeBias <$> n
    Just wI = create . H.fromRows . toList $ VG.convert . L.toVector . N.rNodeIWeights <$> n
    Just wS = create . H.fromRows . toList $ VG.convert . L.toVector . N.rNodeSWeights <$> n
    Just s = create . VG.convert . L.toVector $ s0

networkFromV :: KnownNet i hs o => N.Network i hs o Double -> Network i hs o
networkFromV n = case n of
                   N.NetOL l    -> NetOL (fLayerFromV l)
                   N.NetIL l n' -> rLayerFromV l `NetIL` networkFromV n'
