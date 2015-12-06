{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

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
    _            <*> _            = error "this should never happen"
    {-# INLINE (<*>) #-}

-- instance (KnownNat i, KnownNat o) => Applicative (NetworkU i '[] o) where
--     pure = NetUOL . pure
--     {-# INLINE pure #-}
--     NetUOL f <*> NetUOL x = NetUOL (f <*> x)
--     {-# INLINE (<*>) #-}

-- instance (KnownNat i, KnownNat o, KnownNat j, Applicative (NetworkU j hs o)) => Applicative (NetworkU i (j ': hs) o) where
--     pure x = pure x `NetUIL` pure x
--     {-# INLINE pure #-}
--     NetUIL fi fr <*> NetUIL xi xr = NetUIL (fi <*> xi) (fr <*> xr)
--     {-# INLINE (<*>) #-}

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
                      _ -> error "impossible.  n and ns should be same constructors."
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
            -> a
            -> [(V i a, V o a)]
            -> Network i hs o a
            -> Network i hs o a
trainSeries _ _ _ [] n0 = n0
trainSeries (NA f g) step stepS ios0 n0 =
    case ios0 of
      [] -> n0
      (x0, y0) : ios' -> let (ds, nus, n) = goTS x0 ns0 y0 ios'
                             -- is averaging really the best way?
                             -- can we like, not?
                             -- maybe instead of averaging out the
                             -- differences...add the steps?
                             -- hm, not clear which one is better.
                             -- nuTot = (* (step / fromIntegral n)) <$> foldl'' (liftA2 (+)) (pure 0) nus
                             nuTot = (* (step / fromIntegral n)) <$> nus
                             nuFin = liftA2 (-) nu0 nuTot
                         in  trainStates nuFin ns0 ds
  where
    na'@(NA f_ _) = NA (fst . diff' f) (fst . diff' g)
    (ns0, nu0) = toNetworkU n0
    goTS :: V i a
         -> NetStates i hs o a
         -> V o a
         -> [(V i a, V o a)]
         -> (Deltas i hs o a, NetworkU i hs o a, Int)
    goTS (force-> !x) (force-> !s) (force-> !y) ios =
        case ios of
          [] -> let (force-> !d, force-> !nu) = trainFinal y x s
                in  (d, nu, 1)
          ((x', y'):ios') ->
            let (_ , s' ) = runNetworkU na' nu0 x s
                (force-> !d , force-> !nus, force-> !i) = goTS x' s' y' ios'
                -- can "run" values from runNetworkU be re-used here?
                (d', nu ) = trainSample y' x' s d
            in  (d', liftA2 (+) nu nus, i + 1)
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
                  (delta, ln', shft)   = unzipV3 $ liftA3 (adjustOutput (Node 1 x)) ln y d
                  -- drop contrib from bias term
                  deltaws        :: V j a
                  -- deltaws        = delta *! (nodeWeights <$> ln')
                  deltaws        = delta *! (nodeWeights <$> ln)
                  l'             :: FLayer j o a
                  -- l'             = FLayer ln'
                  l'             = FLayer shft
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
                      (delta, ln', shft) = unzipV3 $ liftA3 (adjustHidden (RNode 1 x s)) ln deltaos' d
                      deltawsI :: V j a
                      deltawsS :: V k a
                      deltawsI = delta *! (rNodeIWeights <$> ln)
                      deltawsS = delta *! (rNodeSWeights <$> ln)
                      -- deltawsI = delta *! (rNodeIWeights <$> ln')
                      -- deltawsS = delta *! (rNodeSWeights <$> ln')
                      l' :: RLayerU j k a
                      -- l' = RLayerU ln'
                      l' = RLayerU shft
                  in  (DeltasIL deltawsI deltawsS deltaos, l' `NetUIL` n'')
                _ -> error "impossible.  nu and ns should be same constructors."
    {-# INLINE trainFinal #-}
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
              (DeltasOL (pure 0), NetUOL (pure 0))
              -- let d              :: V o a
              --     d              = runFLayer l x
              --     delta          :: V o a
              --     ln'            :: V o (Node j a)
              --     (delta, ln', shft)   = unzipV3 $ liftA3 (adjustOutput (Node 1 x)) ln y d
              --     -- drop contrib from bias term
              --     deltaws        :: V j a
              --     -- deltaws        = delta *! (nodeWeights <$> ln')
              --     deltaws        = delta *! (nodeWeights <$> ln)
              --     l'             :: FLayer j o a
              --     l'             = FLayer shft
              -- in  (DeltasOL deltaws, NetUOL l')
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
                          (delta, ln', shft) = unzipV3 $ liftA3 (adjustHidden (RNode 1 x s)) ln deltaos' d
                          deltawsI :: V j a
                          deltawsS :: V k a
                          -- deltawsI = delta *! (rNodeIWeights <$> ln')
                          -- deltawsS = delta *! (rNodeSWeights <$> ln')
                          deltawsI = delta *! (rNodeIWeights <$> ln)
                          deltawsS = delta *! (rNodeSWeights <$> ln)
                          l' :: RLayerU j k a
                          l' = RLayerU shft
                      in  (DeltasIL deltawsI deltawsS deltaos, l' `NetUIL` n'')
                    _ -> error "impossible.  nu and ds should be same constructors."
                _ -> error "impossible.  nu and ns should be same constructors."
    {-# INLINE trainSample #-}
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
                     -- RLayer ln s `NetIL` trainStates nu' ns' ds'
                      -- -- should this have more/less steppage?
                  let s' = liftA2 (\d s0 -> s0 - d * stepS) delS s
                  in  RLayer ln s' `NetIL` trainStates nu' ns' ds'
                _ -> error "impossible.  nu and ds should be same constructors."
            _ -> error "impossible.  nu and ns should be same constructors."
    {-# INLINE trainStates #-}
    adjustOutput :: KnownNat j => Node j a -> Node j a -> a -> a -> (a, Node j a, Node j a)
    adjustOutput xb node y' d = (delta, adjustWeights delta xb node, weightShifts delta xb)
      where
        delta = let (o, o') = diff' g d
                in  (o - y') * o'
    {-# INLINE adjustOutput #-}
    adjustHidden :: Applicative f => f a -> f a -> a -> a -> (a, f a, f a)
    adjustHidden xb node deltao d = (delta, adjustWeights delta xb node, weightShifts delta xb)
      where
        -- instead of (o - target), use deltao, weighted average of errors
        delta = deltao * diff f d
    {-# INLINE adjustHidden #-}
    adjustWeights :: Applicative f => a -> f a -> f a -> f a
    adjustWeights delta = liftA2 (\x n -> n - step * delta * x)
    {-# INLINE adjustWeights #-}
    weightShifts :: Applicative f => a -> f a -> f a
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

-- netUApplicative :: (KnownNat i, KnownNats hs, KnownNat o)
--                 => Proxy i
--                 -> Prod Proxy hs
--                 -> Proxy o
--                 -> Dict (Applicative (NetworkU i hs o))
-- netUApplicative = netInstance (Sub Dict) (Sub Dict)

