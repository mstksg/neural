{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
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
import Control.Lens
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Neural.Types
import Data.Neural.Utility
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Linear
import Linear.V
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
    NetIL :: KnownNat j => !(RLayer i j a) -> !(Network j hs o a) -> Network i (j ': hs) o a

infixr 5 `NetIL`

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
    go :: forall i' hs' o'. (KnownNat i', KnownNat o')
       => Network i' hs' o' a
       -> V i' a
       -> (V o' a, Network i' hs' o' a)
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

runNetStream :: forall i hs o a. (Num a, KnownNat i, KnownNat o)
             => NeuralActs a
             -> Network i hs o a
             -> [V i a]
             -> ([V o a], Network i hs o a)
runNetStream na n vs = runState (mapM (runNetworkS na) vs) n
{-# INLINE runNetStream #-}

runNetStream_ :: forall i hs o a. (Num a, KnownNat i, KnownNat o)
              => NeuralActs a
              -> Network i hs o a
              -> [V i a]
              -> [V o a]
runNetStream_ na = go
  where
    go :: Network i hs o a -> [V i a] -> [V o a]
    go n (v:vs) = let (u, n') = runNetwork na n v
                  in  u : go n' vs
    go _ []     = []
{-# INLINE runNetStream_ #-}

runNetFeedback :: forall i hs a. (Num a, KnownNat i)
               => NeuralActs a
                -> (V i a -> V i a)
               -> Network i hs i a
               -> V i a
               -> [(V i a, Network i hs i a)]
runNetFeedback na nxt n0 i0 = (i0, n0) : go n0 i0
  where
    go :: Network i hs i a -> V i a -> [(V i a, Network i hs i a)]
    go n v = let res@(v', n') = runNetwork na n v
             in  res : go n' (nxt v')
{-# INLINE runNetFeedback #-}

runNetFeedback_ :: forall i hs a. (Num a, KnownNat i)
                => NeuralActs a
                -> (V i a -> V i a)
                -> Network i hs i a
                -> V i a
                -> [V i a]
runNetFeedback_ na nxt n0 i0 = i0 : go n0 i0
  where
    go :: Network i hs i a -> V i a -> [V i a]
    go n v = let (v', n') = runNetwork na n v
             in  v' : go n' (nxt v')
{-# INLINE runNetFeedback_ #-}

randomNetwork :: (MonadRandom m, Random (Network i hs o a), Num a)
              => m (Network i hs o a)
randomNetwork = fmap (subtract 1 . (*2)) <$> getRandom

randomNetwork' :: (KnownNat o, MonadRandom m, Random (Network i hs o a), Num a)
               => m (Network i hs o a)
randomNetwork' = resetNetState <$> randomNetwork

resetNetState :: forall i hs o a. (KnownNat o, Num a) => Network i hs o a -> Network i hs o a
resetNetState n = runIdentity (tNetStates (\_ -> Identity (pure 0)) n)


-- | Some traversals

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


-- | Validating

seriesError :: (KnownNat i, KnownNat o, Num a, Traversable t)
            => NeuralActs a
            -> Network i hs o a
            -> t (V i a, V o a)
            -> (a, Network i hs o a)
seriesError na n ios = runState (seriesErrorS na ios) n
{-# INLINE seriesError #-}

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

instance (KnownNat i, KnownNat o) => Applicative (Network i '[] o) where
    pure = NetOL . pure
    {-# INLINE pure #-}
    NetOL f <*> NetOL x = NetOL (f <*> x)
    {-# INLINE (<*>) #-}

instance (KnownNat i, KnownNat o, KnownNat j, Applicative (Network j hs o)) => Applicative (Network i (j ': hs) o) where
    pure x = pure x `NetIL` pure x
    {-# INLINE pure #-}
    NetIL fi fr <*> NetIL xi xr = NetIL (fi <*> xi) (fr <*> xr)
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


instance (KnownNat i, KnownNat o, Random a) => Random (Network i '[] o a) where
    random = first NetOL . random
    randomR (NetOL rmn, NetOL rmx) = first NetOL . randomR (rmn, rmx)

instance (KnownNat i, KnownNat o, KnownNat j, Random a, Random (Network j hs o a)) => Random (Network i (j ': hs) o a) where
    random g = let (l, g') = random g
               in  first (l `NetIL`) (random g')
    randomR (NetIL lmn nmn, NetIL lmx nmx) g =
        let (l , g') = randomR (lmn, lmx) g
        in  first (l `NetIL`) (randomR (nmn, nmx) g')

instance (KnownNat i, KnownNat o, B.Binary a) => B.Binary (Network i '[] o a) where
    put (NetOL l) = B.put l
    get = NetOL <$> B.get

instance (KnownNat i, KnownNat o, KnownNat j, B.Binary a, B.Binary (Network j hs o a)) => B.Binary (Network i (j ': hs) o a) where
    put (NetIL l n') = B.put l *> B.put n'
    get = NetIL <$> B.get <*> B.get

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
