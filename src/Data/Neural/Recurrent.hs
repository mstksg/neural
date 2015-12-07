{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Data.Neural.Recurrent where

import Control.Applicative
import Control.DeepSeq
import Control.Lens hiding        ((:<))
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Neural.Types
import Data.Neural.Utility
import Data.Proxy
import Data.Reflection
import Data.Type.Product
import GHC.Generics
import GHC.TypeLits
import GHC.TypeLits.List
import Linear
import Linear.V
import Type.Class.Known
import qualified Data.Binary      as B
import qualified Data.Vector      as V

data RNode :: Nat -> Nat -> * -> * where
    RNode :: { rNodeBias     :: !a
             , rNodeIWeights :: !(V i a)
             , rNodeSWeights :: !(V s a)
             } -> RNode i s a
  deriving (Show, Generic, Foldable, Traversable)

data RLayer :: Nat -> Nat -> * -> * where
    RLayer :: { rLayerNodes :: !(V o (RNode i o a))
              , rLayerState :: !(V o a)
              } -> RLayer i o a
  deriving (Show, Functor, Foldable, Traversable, Generic)

data Network :: Nat -> [Nat] -> Nat -> * -> * where
    NetOL :: !(FLayer i o a) -> Network i '[] o a
    NetIL :: (KnownNat j, KnownNats hs) => !(RLayer i j a) -> !(Network j hs o a) -> Network i (j ': hs) o a

infixr 5 `NetIL`

data NetActs :: Nat -> [Nat] -> Nat -> * -> * where
    NetAOL :: !(V o a) -> NetActs i hs o a
    NetAIL :: (KnownNat j, KnownNats hs) => !(V j a) -> !(NetActs j hs o a) -> NetActs i (j ': js) o a

infixr 5 `NetAIL`

data SomeNet :: * -> * where
    SomeNet :: KnownNet i hs o => Network i hs o a -> SomeNet a

data OpaqueNet :: Nat -> Nat -> * -> * where
    OpaqueNet :: KnownNats hs => Network i hs o a -> OpaqueNet i o a


instance Functor (RNode i s) where
    fmap f (RNode b i s) = RNode (f b) (fmap f i) (fmap f s)
    {-# INLINE fmap #-}

instance (Applicative (V i), Applicative (V s)) => Applicative (RNode i s) where
    pure x = RNode x (pure x) (pure x)
    {-# INLINE pure #-}
    RNode fb fi fs <*> RNode xb xi xs = RNode (fb xb) (fi <*> xi) (fs <*> xs)
    {-# INLINE (<*>) #-}

instance (KnownNat i, Additive (V i), Additive (V s)) => Additive (RNode i s) where
    zero = RNode 0 zero zero
    {-# INLINE zero #-}
    RNode b1 i1 s1 ^+^ RNode b2 i2 s2 = RNode (b1 + b2) (i1 ^+^ i2) (s1 ^+^ s2)
    {-# INLINE (^+^) #-}
    RNode b1 i1 s1 ^-^ RNode b2 i2 s2 = RNode (b1 - b2) (i1 ^-^ i2) (s1 ^-^ s2)
    {-# INLINE (^-^) #-}
    lerp a (RNode b1 i1 s1) (RNode b2 i2 s2) = RNode (a * b1 + (1 - a) * b2) (lerp a i1 i2) (lerp a s1 s2)
    {-# INLINE lerp #-}
    liftU2 f (RNode b1 i1 s1) (RNode b2 i2 s2) = RNode (f b1 b2) (liftU2 f i1 i2) (liftU2 f s1 s2)
    {-# INLINE liftU2 #-}
    liftI2 f (RNode b1 i1 s1) (RNode b2 i2 s2) = RNode (f b1 b2) (liftI2 f i1 i2) (liftI2 f s1 s2)
    {-# INLINE liftI2 #-}

instance (KnownNat i, KnownNat s, Random a) => Random (RNode i s a) where
    random = runRand $
        RNode <$> getRandom <*> getRandom <*> getRandom
    randomR (RNode bmn imn smn, RNode bmx imx smx) = runRand $
        RNode <$> getRandomR (bmn, bmx)
              <*> getRandomR (imn, imx)
              <*> getRandomR (smn, smx)

instance NFData a => NFData (RNode i s a)
instance (KnownNat i, KnownNat s, B.Binary a) => B.Binary (RNode i s a)

instance NFData a => NFData (RLayer i o a)

instance (KnownNat i, KnownNat o) => Applicative (RLayer i o) where
    pure x = RLayer (pure (pure x)) (pure x)
    {-# INLINE pure #-}
    RLayer l s <*> RLayer l' s' = RLayer (liftA2 (<*>) l l') (s <*> s')
    {-# INLINE (<*>) #-}

instance (B.Binary a, KnownNat i, KnownNat o) => B.Binary (RLayer i o a)

instance (KnownNat i, KnownNat o, Random a) => Random (RLayer i o a) where
    random = runRand $ RLayer <$> getRandom <*> getRandom
    randomR (RLayer lmn smn, RLayer lmx smx) = runRand $
        RLayer <$> getRandomR (lmn, lmx) <*> getRandomR (smn, smx)

instance NFData a => NFData (NetActs i hs o a) where
    rnf (NetAOL (force -> !_)) = ()
    rnf (NetAIL (force -> !_) (force -> !_)) = ()


runRLayer :: forall i o a. (KnownNat i, KnownNat o, Num a)
          => (a -> a)
          -> RLayer i o a
          -> V i a
          -> (V o a, RLayer i o a)
runRLayer f l v = (v', l { rLayerState = f <$> v' })
  where
    v'       = rLayerNodes l !* RNode 1 v (rLayerState l)
{-# INLINE runRLayer #-}

runRLayerS :: forall i o a m. (KnownNat i, KnownNat o, Num a, MonadState (RLayer i o a) m)
           => (a -> a)
           -> V i a
           -> m (V o a)
runRLayerS f v = state (\l -> runRLayer f l v)
{-# INLINE runRLayerS #-}

runNetwork :: forall i hs o a. (Num a, KnownNat i, KnownNat o)
           => NeuralActs a
           -> Network i hs o a
           -> V i a
           -> (V o a, Network i hs o a)
runNetwork (NA f g) = go
  where
    go :: forall i' hs'. KnownNat i'
       => Network i' hs' o a
       -> V i' a
       -> (V o a, Network i' hs' o a)
    go n v = case n of
               NetOL l    -> (g <$> runFLayer l v, n)
               NetIL l nI -> let (v' , l')  = runRLayer f l v
                                 (v'', nI') = go nI (f <$> v')
                             in  (v'', NetIL l' nI')
{-# INLINE runNetwork #-}

runNetworkS :: (Num a, KnownNat i, KnownNat o, MonadState (Network i hs o a) m)
            => NeuralActs a
            -> V i a
            -> m (V o a)
runNetworkS na v = state (\n -> runNetwork na n v)
{-# INLINE runNetworkS #-}

runNetworkActs :: forall i hs o a. (KnownNat i, Num a)
               => NeuralActs a
               -> Network i hs o a
               -> V i a
               -> (NetActs i hs o a, Network i hs o a)
runNetworkActs (NA f g) = go
  where
    go :: forall i' hs'. KnownNat i'
       => Network i' hs' o a
       -> V i' a
       -> (NetActs i' hs' o a, Network i' hs' o a)
    go n v = case n of
               NetOL l    -> (NetAOL (g <$> runFLayer l v), n)
               NetIL l nI -> let (v' , l') = runRLayer f l v
                                 vRes      = f <$> v'
                                 (nA, nI') = go nI vRes
                             in  (NetAIL vRes nA, NetIL l' nI')
{-# INLINE runNetworkActs #-}

runNetworkActsS :: forall i hs o m a. (KnownNat i, Num a, MonadState (Network i hs o a) m)
                => NeuralActs a
                -> V i a
                -> m (NetActs i hs o a)
runNetworkActsS na v = state (\n -> runNetworkActs na n v)
{-# INLINE runNetworkActsS #-}

runNetStream :: forall i hs o a. (Num a, KnownNat i, KnownNat o)
             => NeuralActs a
             -> Network i hs o a
             -> [V i a]
             -> ([V o a], Network i hs o a)
runNetStream na n vs = runState (mapM (runNetworkS na) vs) n
{-# INLINE runNetStream #-}

runNetStream_ :: forall i hs o a. (Num a, KnownNat i, KnownNat o, NFData a)
              => NeuralActs a
              -> Network i hs o a
              -> [V i a]
              -> [V o a]
runNetStream_ na = go
  where
    go :: Network i hs o a -> [V i a] -> [V o a]
    go n (v:vs) = let (u, n') = runNetwork na n v
                  in  u `deepseq` n' `deepseq` u : go n' vs
    go _ []     = []
{-# INLINE runNetStream_ #-}

runNetStreamActs :: forall i hs o a. (Num a, KnownNat i)
                 => NeuralActs a
                 -> Network i hs o a
                 -> [V i a]
                 -> ([NetActs i hs o a], Network i hs o a)
runNetStreamActs na n vs = runState (mapM (runNetworkActsS na) vs) n
{-# INLINE runNetStreamActs #-}


runNetStreamActs_ :: forall i hs o a. (Num a, KnownNat i, NFData a)
                  => NeuralActs a
                  -> Network i hs o a
                  -> [V i a]
                  -> [NetActs i hs o a]
runNetStreamActs_ na = go
  where
    go :: Network i hs o a -> [V i a] -> [NetActs i hs o a]
    go n (v:vs) = let (u, n') = runNetworkActs na n v
                  in  u `deepseq` n' `deepseq` u : go n' vs
    go _ []     = []
{-# INLINE runNetStreamActs_ #-}



runNetFeedback :: forall i hs o a. (Num a, KnownNat i, KnownNat o)
               => NeuralActs a
               -> (V o a -> V i a)
               -> Network i hs o a
               -> V i a
               -> [(V o a, Network i hs o a)]
runNetFeedback na nxt = go
  where
    go :: Network i hs o a -> V i a -> [(V o a, Network i hs o a)]
    go n v = let res@(v', n') = runNetwork na n v
             in  res : go n' (nxt v')
{-# INLINE runNetFeedback #-}

runNetFeedback_ :: forall i hs o a. (Num a, KnownNat i, KnownNat o)
                => NeuralActs a
                -> (V o a -> V i a)
                -> Network i hs o a
                -> V i a
                -> [V o a]
runNetFeedback_ na nxt = go
  where
    go :: Network i hs o a -> V i a -> [V o a]
    go n v = let (v', n') = runNetwork na n v
             in  v' : go n' (nxt v')
{-# INLINE runNetFeedback_ #-}

runNetFeedbackM_ :: forall i hs o a m. (Num a, KnownNat i, Monad m, KnownNat o)
                 => NeuralActs a
                 -> (V o a -> m (V i a))
                 -> Network i hs o a
                 -> Int
                 -> V i a
                 -> m [V o a]
runNetFeedbackM_ na nxt = go
  where
    go :: Network i hs o a -> Int -> V i a -> m [V o a]
    go n i v | i <= 0    = return []
             | otherwise = do
                 let (v', n') = runNetwork na n v
                 vs <- go n' (i - 1) =<< nxt v'
                 return $ v' : vs

randomNetwork :: (MonadRandom m, Random (Network i hs o a), Num a)
              => m (Network i hs o a)
randomNetwork = fmap (subtract 1 . (*2)) <$> getRandom

randomNetwork' :: (KnownNat o, MonadRandom m, Random (Network i hs o a), Num a)
               => m (Network i hs o a)
randomNetwork' = resetNetState <$> randomNetwork

resetNetState :: forall i hs o a. (KnownNat o, Num a) => Network i hs o a -> Network i hs o a
resetNetState n = runIdentity (tNetStates (\_ -> Identity (pure 0)) n)


-- | Some traversals
-- TODO: sharing with go

tNetRLayers :: (Applicative f, KnownNat o)
            => (forall i' o'. KnownNat o' => RLayer i' o' a -> f (RLayer i' o' a))
            -> Network i hs o a
            -> f (Network i hs o a)
tNetRLayers f n = case n of
                    NetOL _    -> pure n
                    NetIL l n' -> NetIL <$> f l <*> tNetRLayers f n'
{-# INLINE tNetRLayers #-}

tRLayerNodes :: Lens (RLayer i o a)      (RLayer i' o a)
                     (V o (RNode i o a)) (V o (RNode i' o a))
tRLayerNodes f l = (\w -> l { rLayerNodes = w }) <$> f (rLayerNodes l)
{-# INLINE tRLayerNodes #-}

tRLayerState :: Lens' (RLayer i o a) (V o a)
tRLayerState f l = (\s -> l { rLayerState = s }) <$> f (rLayerState l)
{-# INLINE tRLayerState #-}

tNetStates :: (Applicative f, KnownNat o)
           => (forall h. KnownNat h => V h a -> f (V h a))
           -> Network i hs o a
           -> f (Network i hs o a)
tNetStates f = tNetRLayers (tRLayerState f)
{-# INLINE tNetStates #-}

tNetOLayer :: (Functor f, KnownNat i)
           => (forall j. KnownNat j => FLayer j o a -> f (FLayer j o' a))
           -> Network i hs o a
           -> f (Network i hs o' a)
tNetOLayer f n = case n of
                   NetOL l    -> NetOL   <$> f l
                   NetIL l n' -> NetIL l <$> tNetOLayer f n'

tNetLayers :: (Applicative f, KnownNat i)
           => (forall j. KnownNat j => FLayer j o a -> f (FLayer j o' b))
           -> (forall i' j. KnownNat j => RLayer i' j a -> f (RLayer i' j b))
           -> Network i hs o a
           -> f (Network i hs o' b)
tNetLayers f g n = case n of
                     NetOL l    -> NetOL <$> f l
                     NetIL l n' -> NetIL <$> g l <*> tNetLayers f g n'


-- | Validating

seriesError :: (KnownNat i, KnownNat o, Num a, Traversable t)
            => NeuralActs a
            -> Network i hs o a
            -> t (V i a, V o a)
            -> (a, Network i hs o a)
seriesError na n ios = runState (seriesErrorS na ios) n
{-# INLINE seriesError #-}

seriesError_ :: (KnownNat i, KnownNat o, Num a, Traversable t)
             => NeuralActs a
             -> Network i hs o a
             -> t (V i a, V o a)
             -> a
seriesError_ na n ios = evalState (seriesErrorS na ios) n
{-# INLINE seriesError_ #-}


seriesErrorS :: forall i hs o a t m. (KnownNat i, KnownNat o, Num a, MonadState (Network i hs o a) m, Traversable t)
             => NeuralActs a
             -> t (V i a, V o a)
             -> m a
seriesErrorS na = foldM f 0
  where
    f :: a -> (V i a, V o a) -> m a
    f !x (i, o) = do
      res <- runNetworkS na i
      let err2 = o `qd` res
      err2 `seq` return (x + err2)
    {-# INLINE f #-}
{-# INLINE seriesErrorS #-}

-- | Boilerplate instances

instance Functor (Network i hs o) where
    fmap f n = case n of
                 NetOL l -> NetOL (fmap f l)
                 NetIL l n' -> fmap f l `NetIL` fmap f n'
    {-# INLINE fmap #-}

-- instance (KnownNat i, KnownNats hs, KnownNat o, Known (Prod Proxy) hs) => Applicative (Network i hs o) where

instance (KnownNet i hs o) => Applicative (Network i hs o) where
    pure x = case natsList :: NatList hs of
               ØNL     -> NetOL (pure x)
               _ :<# _ -> pure x `NetIL` pure x
    {-# INLINE pure #-}
    NetOL f     <*> NetOL x     = NetOL (f <*> x)
    NetIL fi fr <*> NetIL xi xr = NetIL (fi <*> xi) (fr <*> xr)
    _           <*> _           = error "this should never happen"
    {-# INLINE (<*>) #-}

instance Applicative (Network i hs o) => Additive (Network i hs o) where
    zero = pure 0
    {-# INLINE zero #-}
    (^+^) = liftA2 (+)
    {-# INLINE (^+^) #-}
    (^-^) = liftA2 (-)
    {-# INLINE (^-^) #-}
    liftU2 = liftA2
    {-# INLINE liftU2 #-}
    liftI2 = liftA2
    {-# INLINE liftI2 #-}

instance (Applicative (Network i hs o)) => Metric (Network i hs o)

instance (KnownNet i hs o, Random a) => Random (Network i hs o a) where
    random = runState $ do
      case natsList :: NatList hs of
        ØNL     -> NetOL <$> state random
        _ :<# _ -> NetIL <$> state random <*> state random
    randomR rng = runState $ do
      case rng of
        (NetOL rmn, NetOL rmx)         -> NetOL <$> state (randomR (rmn, rmx))
        (NetIL lmn nmn, NetIL lmx nmx) -> NetIL <$> state (randomR (lmn, lmx))
                                                <*> state (randomR (nmn, nmx))
        (_, _)                         -> error "impossible!"

instance (KnownNat i, KnownNats hs, KnownNat o, Known (Prod Proxy) hs, B.Binary a) => B.Binary (Network i hs o a) where
    put (NetOL l)    = B.put l
    put (NetIL l n') = B.put l *> B.put n'
    get = case natsList :: NatList hs of
            ØNL     -> NetOL <$> B.get
            _ :<# _ -> NetIL <$> B.get <*> B.get

instance NFData a => NFData (Network i hs o a) where
    rnf (NetOL (force -> !_)) = ()
    rnf (NetIL (force -> !_) (force -> !_)) = ()

deriving instance Show a => Show (Network i hs o a)
deriving instance Foldable (Network i hs o)
deriving instance Traversable (Network i hs o)

instance (KnownNat i, KnownNat s) => Nudges (RNode i s) where
    nudges f (RNode b i s) = RNode (RNode (f b) i s)
                                   (flip (RNode b) s <$> nudges f i)
                                   (RNode b i <$> nudges f s)

instance (KnownNat i, KnownNat o) => Nudges (RLayer i o) where
    nudges f (RLayer l s) = RLayer l' (RLayer l <$> nudges f s)
      where
        l' = (fmap.fmap) (`RLayer` s)
           . V . V.generate (dim l) $ \i ->
               let b'  = accumV (\(RNode b iw sw) _ -> RNode (f b) iw sw) l [(i,())]
                   iw' = V . V.generate dimI $ \j -> accumV (\(RNode b iw sw) _ -> RNode b (accumV (\x _ -> f x) iw [(j,())]) sw) l [(i,())]
                   sw' = V . V.generate dimO $ \j -> accumV (\(RNode b iw sw) _ -> RNode b iw (accumV (\x _ -> f x) sw [(j,())])) l [(i,())]
               in  RNode b' iw' sw'
        dimI = reflectDim (Proxy :: Proxy i)
        dimO = reflectDim (Proxy :: Proxy o)

instance (KnownNat i, KnownNat o) => Nudges (Network i hs o) where
    nudges f (NetOL l) = NetOL . fmap NetOL $ nudges f l
    nudges f (NetIL l n) = NetIL (flip NetIL n <$> nudges f l)
                                 (NetIL l      <$> nudges f n)

deriving instance Show a => Show (SomeNet a)
deriving instance Functor SomeNet
deriving instance Foldable SomeNet
deriving instance Traversable SomeNet

instance B.Binary a => B.Binary (SomeNet a) where
    put sn = case sn of
               SomeNet (n :: Network i hs o a) -> do
                 B.put $ natVal (Proxy :: Proxy i)
                 B.put $ natVal (Proxy :: Proxy o)
                 B.put $ OpaqueNet n
    get = do
      i <- B.get
      o <- B.get
      reifyNat i $ \(Proxy :: Proxy i) ->
        reifyNat o $ \(Proxy :: Proxy o) -> do
          oqn <- B.get :: B.Get (OpaqueNet i o a)
          return $ case oqn of
                     OpaqueNet n -> SomeNet n

deriving instance Show a => Show (OpaqueNet i o a)
deriving instance Functor (OpaqueNet i o)
deriving instance Foldable (OpaqueNet i o)
deriving instance Traversable (OpaqueNet i o)

instance (KnownNat i, KnownNat o, B.Binary a) => B.Binary (OpaqueNet i o a) where
    put oqn = case oqn of
                OpaqueNet n -> do
                  case n of
                    NetOL l -> do
                      B.put True
                      B.put l
                    NetIL (l :: RLayer i j a) (n' :: Network j js o a) -> do
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
            l   <- B.get :: B.Get (RLayer i j a)
            nqo <- B.get :: B.Get (OpaqueNet j o a)
            return $ case nqo of
              OpaqueNet n -> OpaqueNet $ l `NetIL` n

asOpaqueNet :: SomeNet a
            -> (forall i o. (KnownNat i, KnownNat o) => OpaqueNet i o a -> r)
            -> r
asOpaqueNet sn f = case sn of
                     SomeNet n -> f (OpaqueNet n)

-- randomNetworkFrom :: (KnownNat i, KnownNats hs, KnownNat o, MonadRandom m, Random a, Num a)
--                   => Proxy i
--                   -> Prod Proxy hs
--                   -> Proxy o
--                   -> m (Network i hs o a)
-- randomNetworkFrom _ hs o =
--     case hs of
--       Ø       -> NetOL . fmap (subtract 1 . (*2)) <$> getRandom
--       j :< js -> do
--         l <- fmap (subtract 1 . (*2)) <$> getRandom
--         n <- randomNetworkFrom j js o
--         return $ l `NetIL` n

-- netApplicative :: (KnownNat i, KnownNats hs, KnownNat o)
--                => Proxy i
--                -> Prod Proxy hs
--                -> Proxy o
--                -> Dict (Applicative (Network i hs o))
-- netApplicative _ hs o = case hs of
--                           Ø       -> Dict
--                           j :< js -> case netApplicative j js o of
--                                        Dict -> Dict

-- netInstance :: forall i hs o f n. (KnownNat i, KnownNats hs, KnownNat o)
--             => (forall i'. (KnownNat i', KnownNat o) :- f (n i' '[] o))
--             -> (forall i' j js. (KnownNat i', KnownNat j, KnownNat o, f (n j js o)) :- f (n i' (j ': js) o))
--             -> Proxy i
--             -> Prod Proxy hs
--             -> Proxy o
--             -> Dict (f (n i hs o))
-- netInstance eBase eRec = go
--   where
--     go :: forall i' hs'. (KnownNat i', KnownNats hs')
--        => Proxy i'
--        -> Prod Proxy hs'
--        -> Proxy o
--        -> Dict (f (n i' hs' o))
--     go _ hs o = case hs of
--                   Ø -> Dict \\ (eBase :: (KnownNat i', KnownNat o) :- f (n i' '[] o))
--                   (j :: Proxy j) :< (js :: Prod Proxy js) ->
--                     case go j js o of
--                       Dict -> Dict \\ (eRec :: (KnownNat i', KnownNat j, KnownNat o, f (n j js o)) :- f (n i' (j ': js) o))
