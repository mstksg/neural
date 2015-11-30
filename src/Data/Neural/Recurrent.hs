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
-- import Control.Arrow
import Control.DeepSeq
import Numeric.AD.Rank1.Forward
import Control.Lens
import Data.Bifunctor
import Data.Proxy
import Control.Monad.Random
import Data.List
import qualified Data.Vector as V
import Control.Monad.State.Strict
import Debug.Trace
import Data.Neural.Types
import Data.Neural.Utility
import GHC.Generics
import GHC.TypeLits
import Linear
import Linear.V
import qualified Data.Binary    as B

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

adjustNetworkGD :: forall i hs o a. (Applicative (Network i hs o), Floating a, KnownNat i, KnownNat o, Show a)
                => NeuralActs a
                -> a
                -> a
                -> [(V i a, V o a)]
                -> Network i hs o a
                -> Network i hs o a
adjustNetworkGD na nudge step ios n0 = n0 ^-^ step *^ signorm nudged
  where
    e0 :: a
    e0 = fst (seriesError na n0 ios)
    nudged :: Network i hs o a
    nudged = subtract e0
           . fst . flip (seriesError na) ios
         <$> nudges (+nudge) n0
    {-# INLINE nudged #-}



-- trainSeries :: forall i hs o a m. (MonadRandom m, MonadState (Network i hs o a) m, Floating a, Ord a, KnownNat i, KnownNat o, Random a)
--             => NeuralActs a
--             -> (Network i hs o a -> m (Network i hs o a))
--             -> a
--             -> a
--             -> [(V i a, V o a)]
--             -> Int
--             -> m ()
-- trainSeries na nudge accept0 accept1 ios n = evalStateT (mapM_ f [0..n]) Nothing
--   where
--     n' :: a
--     n' = fromIntegral n
--     aRange :: a
--     aRange = accept0 - accept1
--     f :: Int -> StateT (Maybe a) m ()
--     f i = StateT $ \lastErr2 ->
--             ((),) . Just <$> adjustNetwork na nudge (accept1 + aRange * fromIntegral i / n') lastErr2 ios
--     {-# INLINE f #-}
-- {-# INLINE trainSeries #-}

trainSeriesGD :: forall i hs o a. (Floating a, KnownNat i, KnownNat o, Random a, Applicative (Network i hs o), Show a, NFData (Network i hs o a))
            => NeuralActs a
            -> a
            -> a
            -> [(V i a, V o a)]
            -> Network i hs o a
            -> Int
            -> Network i hs o a
trainSeriesGD na nudge step ios = iterateN (adjustNetworkGD na nudge step ios)
{-# INLINE trainSeriesGD #-}

newtype RLayerU :: Nat -> Nat -> * -> * where
    RLayerU :: { rLayerUNodes :: (V o (RNode i o a)) } -> RLayerU i o a
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance (KnownNat i, KnownNat o) => Applicative (RLayerU i o) where
    pure x = RLayerU (pure (pure x))
    {-# INLINE pure #-}
    RLayerU l <*> RLayerU l' = RLayerU (liftA2 (<*>) l l')
    {-# INLINE (<*>) #-}

data NetworkU :: Nat -> [Nat] -> Nat -> * -> * where
    NetUOL :: !(FLayer i o a) -> NetworkU i '[] o a
    NetUIL :: KnownNat j => !(RLayerU i j a) -> !(NetworkU j hs o a) -> NetworkU i (j ': hs) o a

deriving instance Functor (NetworkU i hs o)

instance (KnownNat i, KnownNat o) => Applicative (NetworkU i '[] o) where
    pure = NetUOL . pure
    {-# INLINE pure #-}
    NetUOL f <*> NetUOL x = NetUOL (f <*> x)
    {-# INLINE (<*>) #-}

instance (KnownNat i, KnownNat o, KnownNat j, Applicative (NetworkU j hs o)) => Applicative (NetworkU i (j ': hs) o) where
    pure x = pure x `NetUIL` pure x
    {-# INLINE pure #-}
    NetUIL fi fr <*> NetUIL xi xr = NetUIL (fi <*> xi) (fr <*> xr)
    {-# INLINE (<*>) #-}

instance NFData a => NFData (NetworkU i hs o a) where
    rnf (NetUOL (force -> !_)) = ()
    rnf (NetUIL (force -> !_) (force -> !_)) = ()

instance NFData a => NFData (RLayerU i j a)



-- same Layer structure as correpsonding NetworkU, except without the bias
-- term.
data Deltas :: Nat -> [Nat] -> Nat -> * -> * where
    DeltasOL :: !(V i a) -> Deltas i '[] o a
    DeltasIL :: !(V i a) -> !(V j a) -> !(Deltas j hs o a) -> Deltas i (j ': hs) o a

data NetStates :: Nat -> [Nat] -> Nat -> * -> * where
    NetSOL :: NetStates i '[] o a
    NetSIL :: KnownNat j => !(V j a) -> !(NetStates j hs o a) -> NetStates i (j ': hs) o a

runRLayerU :: forall i o a. (KnownNat i, KnownNat o, Num a)
           => (a -> a)
           -> RLayerU i o a
           -> V i a
           -> V o a
           -> (V o a, V o a)
runRLayerU f l v s = (v', f <$> v')
  where
    v'       = rLayerUNodes l !* RNode 1 v s
{-# INLINE runRLayerU #-}

runNetworkU :: forall i hs o a. (Num a, KnownNat i)
            => NeuralActs a
            -> NetworkU i hs o a
            -> V i a
            -> NetStates i hs o a
            -> (V o a, NetStates i hs o a)
runNetworkU (NA f g) = go
  where
    go :: forall j hs'. KnownNat j
       => NetworkU j hs' o a
       -> V j a
       -> NetStates j hs' o a
       -> (V o a, NetStates j hs' o a)
    go n v ns = case n of
                  NetUOL l ->
                    (g <$> runFLayer l v, NetSOL)
                  NetUIL (RLayerU l) n' ->
                    case ns of
                      NetSIL s ns' ->
                        let v' = fmap f (l !* RNode 1 v s)
                            (o, nso) = go n' v' ns'
                        in  (o, NetSIL v' nso)
{-# INLINE runNetworkU #-}

toNetworkU :: Network i hs o a -> (NetStates i hs o a, NetworkU i hs o a)
toNetworkU n = case n of
                 NetOL l    -> (NetSOL, NetUOL l)
                 NetIL l n' -> let (s, n'') = toNetworkU n'
                                   s' = NetSIL (rLayerState l) s
                                   l' = RLayerU (rLayerNodes l)
                               in  (s', NetUIL l' n'')
{-# INLINE toNetworkU #-}

trainSeries :: forall i hs o a. (KnownNat i, KnownNat o, Fractional a, NFData a, Applicative (NetworkU i hs o))
            => NeuralActs (Forward a)
            -> a
            -> [(V i a, V o a)]
            -> Network i hs o a
            -> Network i hs o a
trainSeries _ _ [] n0 = n0
trainSeries (NA f g) step ios0 n0 =
    case ios0 of
      [] -> n0
      (x0, y0) : ios' -> let (ds, nus, n) = goTS x0 ns0 y0 ios'
                             nuAve = (/ fromIntegral n) <$> foldl'' (liftA2 (+)) (pure 0) nus
                         in  trainStates nuAve ns0 ds
  where
    na'@(NA f_ _) = NA (fst . diff' f) (fst . diff' g)
    (ns0, nu0) = toNetworkU n0
    goTS :: V i a
         -> NetStates i hs o a
         -> V o a
         -> [(V i a, V o a)]
         -> (Deltas i hs o a, [NetworkU i hs o a], Int)
    goTS x s y ios =
        case ios of
          [] -> let (d, nu) = trainFinal y x s
                in  (d, [nu], 1)
          ((x', y'):ios') ->
            let (_ , s' ) = runNetworkU na' nu0 x s
                (d , nus, i) = goTS x' s' y' ios'
                -- can "run" values from runNetworkU be re-used here?
                (d', nu ) = trainSample y' x' s d
            in  (d', nu : nus, i + 1)
    trainFinal :: V o a
               -> V i a
               -> NetStates i hs o a
               -> (Deltas i hs o a, NetworkU i hs o a)
    trainFinal y = go nu0
      where
        go :: forall j hs'. KnownNat j
           => NetworkU j hs' o a
           -> V j a
           -> NetStates j hs' o a
           -> (Deltas j hs' o a, NetworkU j hs' o a)
        go nu x ns =
          case nu of
            NetUOL l@(FLayer ln) ->
              let d              :: V o a
                  d              = runFLayer l x
                  delta          :: V o a
                  ln'            :: V o (Node j a)
                  (delta, ln')   = unzipV $ liftA3 (adjustOutput (Node 1 x)) ln y d
                  -- drop contrib from bias term
                  deltaws        :: V j a
                  deltaws        = delta *! (nodeWeights <$> ln')
                  l'             :: FLayer j o a
                  l'             = FLayer ln'
              in  (DeltasOL deltaws, NetUOL l')
            NetUIL l@(RLayerU ln :: RLayerU j k a) (nu' :: NetworkU k ks o a) ->
              case ns of
                NetSIL s ns' ->
                  let d, s', o :: V k a
                      (d, s') = runRLayerU f_ l x s
                      o = s'
                      deltaos :: Deltas k ks o a
                      n'' :: NetworkU k ks o a
                      (deltaos, n'') = go nu' o ns'
                      -- deltaos from inputs only, not state
                      deltaos' :: V k a
                      deltaos' = case deltaos of
                                   DeltasOL dos -> dos
                                   DeltasIL dos _ _ -> dos
                      delta :: V k a
                      ln' :: V k (RNode j k a)
                      (delta, ln') = unzipV $ liftA3 (adjustHidden (RNode 1 x s)) ln deltaos' d
                      deltawsI :: V j a
                      deltawsS :: V k a
                      deltawsI = delta *! (rNodeIWeights <$> ln')
                      deltawsS = delta *! (rNodeSWeights <$> ln')
                      l' :: RLayerU j k a
                      l' = RLayerU ln'
                  in  (DeltasIL deltawsI deltawsS deltaos, l' `NetUIL` n'')
    trainSample :: V o a
                -> V i a
                -> NetStates i hs o a
                -> Deltas i hs o a
                -> (Deltas i hs o a, NetworkU i hs o a)
    trainSample y = go nu0
      where
        go :: forall j hs'. KnownNat j
           => NetworkU j hs' o a
           -> V j a
           -> NetStates j hs' o a
           -> Deltas j hs' o a
           -> (Deltas j hs' o a, NetworkU j hs' o a)
        go nu x ns ds =
          case nu of
            NetUOL l@(FLayer ln) ->
              let d              :: V o a
                  d              = runFLayer l x
                  delta          :: V o a
                  ln'            :: V o (Node j a)
                  (delta, ln')   = unzipV $ liftA3 (adjustOutput (Node 1 x)) ln y d
                  -- drop contrib from bias term
                  deltaws        :: V j a
                  deltaws        = delta *! (nodeWeights <$> ln')
                  l'             :: FLayer j o a
                  l'             = FLayer ln'
              in  (DeltasOL deltaws, NetUOL l')
            NetUIL l@(RLayerU ln :: RLayerU j k a) (nu' :: NetworkU k ks o a) ->
              case ns of
                NetSIL s ns' ->
                  case ds of
                    DeltasIL _ (delS :: V k a) ds' ->
                      let d, s', o :: V k a
                          (d, s') = runRLayerU f_ l x s
                          o = s'
                          deltaos :: Deltas k ks o a
                          n'' :: NetworkU k ks o a
                          (deltaos, n'') = go nu' o ns' ds'
                          -- deltaos from inputs only, not state
                          deltaos' :: V k a
                          deltaos' = case deltaos of            -- yeaa :D
                                       DeltasOL dos     -> dos ^+^ delS
                                       DeltasIL dos _ _ -> dos ^+^ delS
                          delta :: V k a
                          ln' :: V k (RNode j k a)
                          (delta, ln') = unzipV $ liftA3 (adjustHidden (RNode 1 x s)) ln deltaos' d
                          deltawsI :: V j a
                          deltawsS :: V k a
                          deltawsI = delta *! (rNodeIWeights <$> ln')
                          deltawsS = delta *! (rNodeSWeights <$> ln')
                          l' :: RLayerU j k a
                          l' = RLayerU ln'
                      in  (DeltasIL deltawsI deltawsS deltaos, l' `NetUIL` n'')
    trainStates :: forall j hs'. KnownNat j
                => NetworkU j hs' o a
                -> NetStates j hs' o a
                -> Deltas j hs' o a
                -> Network j hs' o a
    trainStates nu ns ds =
      case nu of
        NetUOL l -> NetOL l
        NetUIL (RLayerU ln :: RLayerU j k a) (nu' :: NetworkU k ks o a) ->
          case ns of
            NetSIL s ns' ->
              case ds of
                DeltasIL _ (delS :: V k a) ds' ->
                      -- should this have more/less steppage?
                  let s' = liftA2 (\d s0 -> s0 - d * step) delS s
                  in  RLayer ln s' `NetIL` trainStates nu' ns' ds'
    adjustOutput :: KnownNat j => Node j a -> Node j a -> a -> a -> (a, Node j a)
    adjustOutput xb node y' d = (delta, adjustWeights delta xb node)
      where
        delta = let (o, o') = diff' g d
                in  (o - y') * o'
    {-# INLINE adjustOutput #-}
    adjustHidden :: Applicative f => f a -> f a -> a -> a -> (a, f a)
    adjustHidden xb node deltao d = (delta, adjustWeights delta xb node)
      where
        -- instead of (o - target), use deltao, weighted average of errors
        delta = deltao * diff f d
    {-# INLINE adjustHidden #-}
    -- is this correct? should it be x and not y?
    adjustWeights :: Applicative f => a -> f a -> f a -> f a
    adjustWeights delta = liftA2 (\x n -> n - step * delta * x)
    {-# INLINE adjustWeights #-}


    -- go :: forall j hs'. KnownNat j => [(V j a, V o a)] -> Network j hs' o a -> (V j a, [Network j hs' o a])
    -- go ios n = undefined
  -- where
  --   go :: forall j hs'. KnownNat j => [(V j a, V o a)] -> Network j hs' o a -> (V j a, [Network j hs' o a])
  --   go ios n = case ios of
  --                []              -> (pure 0, [])
  --                ((x, y) : ios') ->
  --                  case n of
  --                    NetOL l@(FLayer ln) ->
  --                      let d :: V o a
  --                          d = runFLayer l x
  --                          delta :: V o a
  --                          ln' :: V o (Node j a)
  --                          (delta, ln') = unzipV $ liftA3 (adjustOutput (Node 1 x)) ln y d
  --                          -- drop contrib from bias terms
  --                          deltaws :: V j a
  --                          deltaws = delta *! (nodeWeights <$> ln')
  --                          l' :: FLayer j o a
  --                          l' = FLayer ln'
  --                          rest :: [Network j hs' o a]
  --                          rest = snd $ go ios' n
  --                      in  (deltaws, NetOL l' : rest)
  --                    NetIL l@(RLayer ln ls :: RLayer j k a) (n' :: Network k ks o a) ->
  --                      let d :: V k a
  --                          (d, l@(RLayer ln' ls')) = runRLayer (fst . diff' f) l x
  --                          o :: V k a
  --                          o = fst . diff' f <$> d
  --                          deltaos :: V k a
  --                          n'' :: [Network k ks o a]
  --                          (deltaos, n'') = goO o n'
  --                      in  undefined
  --   goO :: forall j hs'. KnownNat j => V j a -> FLayer j o a -> (V j a, FLayer j o a)
  --   goO x l@(FLayer ln) = undefined
  --   -- NetIL :: KnownNat j => !(RLayer i j a) -> !(Network j hs o a) -> Network i (j ': hs) o a
  --   adjustOutput :: KnownNat j => Node j a -> Node j a -> a -> a -> (a, Node j a)
  --   adjustOutput xb node y' d = (delta, adjustWeights delta xb node)
  --     where
  --       delta = let (o, o') = diff' g d
  --               in  (o - y') * o'
  --   {-# INLINE adjustOutput #-}
  --   adjustWeights :: KnownNat j => a -> Node j a -> Node j a -> Node j a
  --   adjustWeights delta = liftA2 (\w n -> n - step * delta * w)
  --   {-# INLINE adjustWeights #-}
          -- let d :: V k a
          --     d                    = runFLayer l x
          --     o :: V k a
          --     o                    = fst . diff' f <$> d
          --     deltaos :: V k a
          --     n'' :: Network k ks o a
          --     (deltaos, n'')       = go o n'
          --     delta :: V k a
          --     ln' :: V k (Node j a)
          --     (delta, ln')         = unzipV $ liftA3 (adjustHidden xb) ln deltaos d
          --     deltaws :: V j a
          --     deltaws              = delta *! (nodeWeights <$> ln')
          --     l' :: FLayer j k a
          --     l'                   = FLayer ln'
                   -- NetOL :: !(FLayer i o a) -> Network i '[] o a

    -- go :: forall j hs'. KnownNat j => V j a -> Network j hs' o a -> (V j a, Network j hs' o a)

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
