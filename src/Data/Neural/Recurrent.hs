{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}

module Data.Neural.Recurrent where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Lens
import Control.Monad.Random
import Control.Monad.State
import Data.Neural.Types hiding    (Network)
import Data.Neural.Utility
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Linear
import Linear.V
import Type.Class.Witness
import qualified Data.Binary       as B
import qualified Data.Neural.Types as N
import qualified Data.Vector       as V

data RLayer :: Nat -> Nat -> * -> * where
    RLayer :: { rLayerWeights :: !(FLayer (i + o) o a)
              , rLayerState   :: !(V o a)
              } -> RLayer i o a
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (RLayer i o a)

type Network = N.Network RLayer

instance (KnownNat i, KnownNat o) => Applicative (RLayer i o) where
    pure x =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
        Wit -> RLayer (pure x) (pure x)
    {-# INLINE pure #-}
    RLayer l s <*> RLayer l' s' =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
        Wit -> RLayer (l <*> l') (s <*> s')
    {-# INLINE (<*>) #-}

instance (B.Binary a, KnownNat i, KnownNat o) => B.Binary (RLayer i o a) where
    put (RLayer l s) =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
        Wit -> B.put l *> B.put s
    get =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
        Wit -> RLayer <$> B.get <*> B.get

instance (KnownNat i, KnownNat o, Random a) => Random (RLayer i o a) where
    random g =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
        Wit -> flip runState g $
                 RLayer <$> state random <*> state random
    randomR (RLayer l s, RLayer l' s') g =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
        Wit -> flip runState g $
                 RLayer <$> state (randomR (l, l'))
                        <*> state (randomR (s, s'))

runRLayer :: forall i o a. (KnownNat i, KnownNat o, Num a)
          => (a -> a)
          -> RLayer i o a
          -> V i a
          -> (V o a, RLayer i o a)
runRLayer f l v =
    case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
      Wit -> go
  where
    go :: KnownNat (i + o) => (V o a, RLayer i o a)
    go = (v', l { rLayerState = newState })
      where
        -- not verified by compiler.  beware!
        -- also, is concatting to slow?
        vInp :: V (i + o) a
        vInp = V (toVector v V.++ toVector (rLayerState l))
        v' :: V o a
        v' = runFLayer (rLayerWeights l) vInp
        newState :: V o a
        newState = f <$> v'
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
               NetOL l    -> (fmap g *** NetOL) (runRLayer f l v)
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
               -> Network i hs i a
               -> V i a
               -> [(V i a, Network i hs i a)]
runNetFeedback na = go
  where
    go :: Network i hs i a -> V i a -> [(V i a, Network i hs i a)]
    go n v = let res@(v', n') = runNetwork na n v
             in  res : go n' v'
{-# INLINE runNetFeedback #-}

runNetFeedback_ :: forall i hs a. (Num a, KnownNat i)
                => NeuralActs a
                -> Network i hs i a
                -> V i a
                -> [V i a]
runNetFeedback_ na = go
  where
    go :: Network i hs i a -> V i a -> [V i a]
    go n v = let (v', n') = runNetwork na n v
             in  v' : go n' v'
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
            => (forall i o. KnownNat o => RLayer i o a -> f (RLayer i o b))
            -> Network i hs o a
            -> f (Network i hs o b)
tNetRLayers f n = case n of
                    NetOL l    -> NetOL <$> f l
                    NetIL l n' -> NetIL <$> f l <*> tNetRLayers f n'
{-# INLINE tNetRLayers #-}

tRLayerWeights :: Lens (RLayer i o a) (RLayer i' o a)
                       (FLayer (i + o) o a) (FLayer (i' + o) o a)
tRLayerWeights f l = (\w -> l { rLayerWeights = w }) <$> f (rLayerWeights l)
{-# INLINE tRLayerWeights #-}

tRLayerState :: Lens' (RLayer i o a) (V o a)
tRLayerState f l = (\s -> l { rLayerState = s }) <$> f (rLayerState l)
{-# INLINE tRLayerState #-}

tNetStates :: (Applicative f, KnownNat o)
           => (forall h. KnownNat h => V h a -> f (V h a))
           -> Network i hs o a
           -> f (Network i hs o a)
tNetStates f = tNetRLayers (tRLayerState f)
{-# INLINE tNetStates #-}


-- | Training

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

nudgeNetworkN :: (MonadRandom m, Floating a, Random (Network i hs o a), Applicative (Network i hs o))
              => a -> Network i hs o a -> m (Network i hs o a)
nudgeNetworkN dr n = (n ^+^) . (dr *^)
                   . signorm
                 <$> randomNetwork
{-# INLINE nudgeNetworkN #-}

nudgeNetwork :: forall i hs o a m. (MonadRandom m, Num a, Random a) => a -> Network i hs o a -> m (Network i hs o a)
nudgeNetwork dr = traverse f
  where
    f :: a -> m a
    f !x = (+ x) <$> getRandomR (-dr, dr)
{-# INLINE nudgeNetwork #-}

adjustNetwork :: (MonadRandom m, MonadState (Network i hs o a) m, Random a, Ord a, Floating a, KnownNat i, KnownNat o)
              => NeuralActs a
              -> (Network i hs o a -> m (Network i hs o a))
              -> a
              -> Maybe a
              -> [(V i a, V o a)]
              -> m a
adjustNetwork na nudge accept e0 ios = do
    nudged <- nudge =<< get
    err2   <- case e0 of
                Just x  -> return x
                Nothing -> gets (\n -> fst (seriesError na n ios))
    let (err2', _) = seriesError na nudged ios
        thresh = exp (err2 - err2')     -- smaller: worse
    if err2' < err2
      then state $ \_ -> (err2', nudged)
      else do
        choice <- getRandom
        if choice * accept < thresh
          then state $ \_ -> (err2', nudged)
          else return err2
{-# INLINE adjustNetwork #-}


trainSeries :: forall i hs o a m. (MonadRandom m, MonadState (Network i hs o a) m, Floating a, Ord a, KnownNat i, KnownNat o, Random a)
            => NeuralActs a
            -> (Network i hs o a -> m (Network i hs o a))
            -> a
            -> a
            -> [(V i a, V o a)]
            -> Int
            -> m ()
trainSeries na nudge accept0 accept1 ios n = evalStateT (mapM_ f [0..n]) Nothing
  where
    n' :: a
    n' = fromIntegral n
    aRange :: a
    aRange = accept0 - accept1
    f :: Int -> StateT (Maybe a) m ()
    f i = StateT $ \lastErr2 ->
            ((),) . Just <$> adjustNetwork na nudge (accept1 + aRange * fromIntegral i / n') lastErr2 ios
    {-# INLINE f #-}
{-# INLINE trainSeries #-}



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

