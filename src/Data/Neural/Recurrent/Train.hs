{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
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

module Data.Neural.Recurrent.Train where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Neural.Recurrent
import Data.Neural.Types
import Data.Neural.Utility
import GHC.Generics
import GHC.TypeLits
import GHC.TypeLits.List
import Linear
import Linear.V
import Numeric.AD.Rank1.Forward

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
    NetUIL :: (KnownNat j, KnownNats hs) => !(RLayerU i j a) -> !(NetworkU j hs o a) -> NetworkU i (j ': hs) o a

deriving instance Functor (NetworkU i hs o)

instance (KnownNet i hs o) => Applicative (NetworkU i hs o) where
    pure x = case natsList :: NatList hs of
               ØNL     -> NetUOL (pure x)
               _ :<# _ -> pure x `NetUIL` pure x
    {-# INLINE pure #-}
    NetUOL f     <*> NetUOL x     = NetUOL (f <*> x)
    NetUIL fi fr <*> NetUIL xi xr = NetUIL (fi <*> xi) (fr <*> xr)
    {-# INLINE (<*>) #-}

instance Applicative (NetworkU i hs o) => Additive (NetworkU i hs o) where
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
    NetSIL :: (KnownNat j, KnownNats hs) => !(V j a) -> !(NetStates j hs o a) -> NetStates i (j ': hs) o a

instance NFData a => NFData (NetStates i hs o a) where
    rnf NetSOL = ()
    rnf (NetSIL (force -> !_) (force -> !_)) = ()

instance NFData a => NFData (Deltas i hs o a) where
    rnf (DeltasOL (force -> !_)) = ()
    rnf (DeltasIL (force -> !_) (force -> !_) (force -> !_)) = ()

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

tNetULayers :: forall i hs o o' a b f. (Applicative f)
            => (forall j. FLayer j o a -> f (FLayer j o' b))
            -> (forall i' j. RLayerU i' j a -> f (RLayerU i' j b))
            -> NetworkU i hs o a
            -> f (NetworkU i hs o' b)
tNetULayers f g = go
  where
    go :: forall j js. NetworkU j js o a -> f (NetworkU j js o' b)
    go n = case n of
             NetUOL l    -> NetUOL <$> f l
             NetUIL l n' -> NetUIL <$> g l <*> go n'

toNetworkU :: Network i hs o a -> (NetStates i hs o a, NetworkU i hs o a)
toNetworkU n = case n of
                 NetOL l    -> (NetSOL, NetUOL l)
                 NetIL l n' -> let (s, n'') = toNetworkU n'
                                   s' = NetSIL (rLayerState l) s
                                   l' = RLayerU (rLayerNodes l)
                               in  (s', NetUIL l' n'')
{-# INLINE toNetworkU #-}

trainSeries :: forall i hs o a. (KnownNet i hs o, Fractional a, NFData a)
            => NeuralActs (Forward a)
            -> a
            -> a
            -> V o a
            -> [V i a]
            -> Network i hs o a
            -> Network i hs o a
trainSeries (NA f g) step stepS y inps0 n0 =
    case inps0 of
      [] -> n0
      x0:xs -> let (ds, nuShifts) = goTS x0 ns0 xs
                   nu1 = nu0 ^-^ step *^ nuShifts
               in  trainStates nu1 ns0 ds
  where
    na'@(NA f_ _) = NA (fst . diff' f) (fst . diff' g)
    (ns0, nu0) = toNetworkU n0
    goTS :: V i a
         -> NetStates i hs o a
         -> [V i a]
         -> (Deltas i hs o a, NetworkU i hs o a)
    goTS (force-> !x) (force-> !s) inps =
        case inps of
          []    -> let (force-> !d, force-> !nu) = trainFinal x s
                   in  (d, nu)
          x':xs ->
            let (_ , s') = runNetworkU na' nu0 x s
                (force-> !d , force-> !nus) = goTS x' s' xs
                -- can "run" values from runNetworkU be re-used here?
                (d', nu) = trainSample x' s d
            in  (d', nu ^+^ nus)
    trainFinal :: V i a
               -> NetStates i hs o a
               -> (Deltas i hs o a, NetworkU i hs o a)
    trainFinal = go nu0
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
                  (delta, shft)   = unzipV $ liftA2 (adjustOutput (Node 1 x)) y d
                  -- drop contrib from bias term
                  deltaws        :: V j a
                  deltaws        = delta *! (nodeWeights <$> ln)
              in  (DeltasOL deltaws, NetUOL (FLayer shft))
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
                      (delta, shft) = unzipV $ liftA2 (adjustHidden (RNode 1 x s)) deltaos' d
                      deltawsI :: V j a
                      deltawsS :: V k a
                      deltawsI = delta *! (rNodeIWeights <$> ln)
                      deltawsS = delta *! (rNodeSWeights <$> ln)
                  in  (DeltasIL deltawsI deltawsS deltaos, RLayerU shft `NetUIL` n'')
    {-# INLINE trainFinal #-}
    trainSample :: V i a
                -> NetStates i hs o a
                -> Deltas i hs o a
                -> (Deltas i hs o a, NetworkU i hs o a)
    trainSample = go nu0
      where
        go :: forall j hs'. KnownNat j
           => NetworkU j hs' o a
           -> V j a
           -> NetStates j hs' o a
           -> Deltas j hs' o a
           -> (Deltas j hs' o a, NetworkU j hs' o a)
        go nu x ns ds =
          case nu of
            NetUOL _ ->
              (DeltasOL zero, NetUOL zero)
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
                                       DeltasOL _       -> delS
                                       DeltasIL dos _ _ -> dos ^+^ delS
                          delta :: V k a
                          (delta, shft) = unzipV $ liftA2 (adjustHidden (RNode 1 x s)) deltaos' d
                          deltawsI :: V j a
                          deltawsS :: V k a
                          deltawsI = delta *! (rNodeIWeights <$> ln)
                          deltawsS = delta *! (rNodeSWeights <$> ln)
                      in  (DeltasIL deltawsI deltawsS deltaos, RLayerU shft `NetUIL` n'')
    {-# INLINE trainSample #-}
    trainStates :: forall j hs'. ()
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
                  let s' = liftA2 (\d s0 -> s0 - d * stepS) delS s
                  in  RLayer ln s' `NetIL` trainStates nu' ns' ds'
    {-# INLINE trainStates #-}
    adjustOutput :: Node j a -> a -> a -> (a, Node j a)
    adjustOutput xb y' d = (delta, weightShifts delta xb)
      where
        delta = let (o, o') = diff' g d
                in  (o - y') * o'
    {-# INLINE adjustOutput #-}
    adjustHidden :: RNode j k a -> a -> a -> (a, RNode j k a)
    adjustHidden xb deltao d = (delta, weightShifts delta xb)
      where
        -- instead of (o - target), use deltao, weighted average of errors
        delta = deltao * diff f d
    {-# INLINE adjustHidden #-}
    weightShifts :: Functor f => a -> f a -> f a
    weightShifts delta = fmap (\x -> delta * x)
    {-# INLINE weightShifts #-}

-- | Stochastic

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

adjustNetworkGD :: forall i hs o a. (Applicative (Network i hs o), Floating a, KnownNat i, KnownNat o)
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



trainSeriesSI :: forall i hs o a m. (MonadRandom m, MonadState (Network i hs o a) m, Floating a, Ord a, KnownNat i, KnownNat o, Random a)
              => NeuralActs a
              -> (Network i hs o a -> m (Network i hs o a))
              -> a
              -> a
              -> [(V i a, V o a)]
              -> Int
              -> m ()
trainSeriesSI na nudge accept0 accept1 ios n = evalStateT (mapM_ f [0..n]) Nothing
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

trainSeriesGD :: forall i hs o a. (Floating a, KnownNet i hs o, NFData a)
            => NeuralActs a
            -> a
            -> a
            -> [(V i a, V o a)]
            -> Network i hs o a
            -> Int
            -> Network i hs o a
trainSeriesGD na nudge step ios = iterateN (adjustNetworkGD na nudge step ios)
{-# INLINE trainSeriesGD #-}

-- netUApplicative :: (KnownNat i, KnownNats hs, KnownNat o)
--                 => Proxy i
--                 -> Prod Proxy hs
--                 -> Proxy o
--                 -> Dict (Applicative (NetworkU i hs o))
-- netUApplicative = netInstance (Sub Dict) (Sub Dict)

