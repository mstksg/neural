{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Neural.HMatrix.Recurrent.Train where

import Control.DeepSeq
import Data.Neural.HMatrix.Recurrent
import Data.Neural.Types             (NeuralActs(..), KnownNet)
import GHC.Generics                  (Generic)
import GHC.TypeLits
import GHC.TypeLits.List
import Numeric.AD.Rank1.Forward
import Numeric.LinearAlgebra.Static

data RLayerU :: Nat -> Nat -> * where
    RLayerU :: { rLayerUBiases   :: !(R o)
               , rLayerUIWeights :: !(L o i)
               , rLayerUSWeights :: !(L o o)
               } -> RLayerU i o
  deriving (Show, Generic)

data NetworkU :: Nat -> [Nat] -> Nat -> * where
    NetUOL :: !(FLayer i o) -> NetworkU i '[] o
    NetUIL :: (KnownNat j, KnownNats hs)
           => !(RLayerU i j) -> !(NetworkU j hs o) -> NetworkU i (j ': hs) o

data Deltas :: Nat -> [Nat] -> Nat -> * where
    DeltasOL :: !(R i) -> Deltas i '[] o
    DeltasIL :: !(R i) -> !(R j) -> !(Deltas j hs o) -> Deltas i (j ': hs) o

data NetStates :: Nat -> [Nat] -> Nat -> * where
    NetSOL :: NetStates i '[] o
    NetSIL :: (KnownNat j, KnownNats hs)
           => !(R j) -> !(NetStates j hs o) -> NetStates i (j ': hs) o

instance NFData (RLayerU i j)

instance NFData (NetworkU i hs o) where
    rnf (NetUOL (force -> !_)) = ()
    rnf (NetUIL (force -> !_) (force -> !_)) = ()

instance NFData (NetStates i hs o) where
    rnf NetSOL = ()
    rnf (NetSIL (force -> !_) (force -> !_)) = ()

instance NFData (Deltas i hs o) where
    rnf (DeltasOL (force -> !_)) = ()
    rnf (DeltasIL (force -> !_) (force -> !_) (force -> !_)) = ()

runRLayerU :: (KnownNat i, KnownNat o)
           => (Double -> Double)
           -> RLayerU i o
           -> R i
           -> R o
           -> (R o, R o)
runRLayerU f (RLayerU b wI wS) v s = (v', mapR f v')
  where
    v'       = b + wI #> v + wS #> s
{-# INLINE runRLayerU #-}

runNetworkU :: forall i hs o. (KnownNet i hs o)
            => NeuralActs Double
            -> NetworkU i hs o
            -> R i
            -> NetStates i hs o
            -> (R o, NetStates i hs o)
runNetworkU (NA f g) = go
  where
    go :: forall j hs'. KnownNat j
       => NetworkU j hs' o
       -> R j
       -> NetStates j hs' o
       -> (R o, NetStates j hs' o)
    go n v ns = case n of
                  NetUOL l ->
                    (mapR g (runFLayer l v), NetSOL)
                  NetUIL (RLayerU b wI wS) n' ->
                    case ns of
                      NetSIL s ns' ->
                        let v' = mapR f $ b + wI #> v + wS #> s
                            (o, nso) = go n' v' ns'
                        in  (o, NetSIL v' nso)
                      _ -> error "impossible.  n and ns should be same constructors."
{-# INLINE runNetworkU #-}

toNetworkU :: Network i hs o -> (NetStates i hs o, NetworkU i hs o)
toNetworkU n = case n of
                 NetOL l    -> (NetSOL, NetUOL l)
                 NetIL l n' -> let (s, n'') = toNetworkU n'
                                   s' = NetSIL (rLayerState l) s
                                   l' = RLayerU (rLayerBiases l)
                                                (rLayerIWeights l)
                                                (rLayerSWeights l)
                               in  (s', NetUIL l' n'')
{-# INLINE toNetworkU #-}

-- trainSeries :: forall i hs o. KnownNet i hs o
--             => NeuralActs (Forward Double)
--             -> Double
--             -> Double
--             -> R o
--             -> [R i]
--             -> Network i hs o
--             -> Network i hs o
-- trainSeries (NA f g) step stepS y inps0 n0 =
--     case inps0 of
--       [] -> n0
--       x0:xs -> let (ds, nuShifts) = goTS x0 ns0 xs
--                    nu1 = nu0 - step *^ nuShifts
--                in  trainStates nu1 ns0 ds
--   where
--     na'@(NA f_ _) = NA (fst . diff' f) (fst . diff' g)
--     (ns0, nu0) = toNetworkU n0
--     goTS :: R i
--          -> NetStates i hs o
--          -> [R i]
--          -> (Deltas i hs o, NetworkU i hs o)
--     goTS (force-> !x) (force-> !s) inps =
--         case inps of
--           []    -> let (force-> !d, force-> !nu) = trainFinal x s
--                    in  (d, nu)
--           x':xs ->
--             let (_ , s') = runNetworkU na' nu0 x s
--                 (force-> !d , force-> !nus) = goTS x' s' xs
--                 -- can "run" values from runNetworkU be re-used here?
--                 (d', nu) = trainSample x' s d
--             in  (d', nu + nus)
--     trainFinal :: R i
--                -> NetStates i hs o
--                -> (Deltas i hs o, NetworkU i hs o)
--     trainFinal = undefined
--     trainSample :: R i
--                 -> NetStates i hs o
--                 -> Deltas i hs o
--                 -> (Deltas i hs o, NetworkU i hs o)
--     trainSample = undefined
--     -- trainFinal :: R i
--     --            -> NetStates i hs o
--     --            -> (Deltas i hs o, NetworkU i hs o)
--     -- trainFinal = go nu0
--     --   where
--     --     go :: forall j hs'. KnownNat j
--     --        => NetworkU j hs' o
--     --        -> R j
--     --        -> NetStates j hs' o
--     --        -> (Deltas j hs' o, NetworkU j hs' o)
--     --     go nu x ns =
--     --       case nu of
--     --         NetUOL l@(FLayer ln) ->
--     --           let d              :: R o
--     --               d              = runFLayer l x
--     --               delta          :: R o
--     --               (delta, shft)   = unzipV $ liftA2 (adjustOutput (Node 1 x)) y d
--     --               -- drop contrib from bias term
--     --               deltaws        :: R j
--     --               deltaws        = delta *! (nodeWeights <$> ln)
--     --           in  (DeltasOL deltaws, NetUOL (FLayer shft))
--     --         NetUIL l@(RLayerU ln :: RLayerU j k) (nu' :: NetworkU k ks o) ->
--     --           case ns of
--     --             NetSIL s ns' ->
--     --               let d, s', o :: R k
--     --                   (d, s') = runRLayerU f_ l x s
--     --                   o = s'
--     --                   deltaos :: Deltas k ks o
--     --                   n'' :: NetworkU k ks o
--     --                   (deltaos, n'') = go nu' o ns'
--     --                   -- deltaos from inputs only, not state
--     --                   deltaos' :: R k
--     --                   deltaos' = case deltaos of
--     --                                DeltasOL dos -> dos
--     --                                DeltasIL dos _ _ -> dos
--     --                   delta :: R k
--     --                   (delta, shft) = unzipV $ liftA2 (adjustHidden (RNode 1 x s)) deltaos' d
--     --                   deltawsI :: R j
--     --                   deltawsS :: R k
--     --                   deltawsI = delta *! (rNodeIWeights <$> ln)
--     --                   deltawsS = delta *! (rNodeSWeights <$> ln)
--     --               in  (DeltasIL deltawsI deltawsS deltaos, RLayerU shft `NetUIL` n'')
--     --             _ -> error "impossible.  nu and ns should be same constructors."
--     -- {-# INLINE trainFinal #-}
--     -- trainSample :: R i
--     --             -> NetStates i hs o
--     --             -> Deltas i hs o
--     --             -> (Deltas i hs o, NetworkU i hs o)
--     -- trainSample = go nu0
--     --   where
--     --     go :: forall j hs'. KnownNat j
--     --        => NetworkU j hs' o
--     --        -> R j
--     --        -> NetStates j hs' o
--     --        -> Deltas j hs' o
--     --        -> (Deltas j hs' o, NetworkU j hs' o)
--     --     go nu x ns ds =
--     --       case nu of
--     --         NetUOL _ ->
--     --           (DeltasOL zero, NetUOL zero)
--     --         NetUIL l@(RLayerU ln :: RLayerU j k) (nu' :: NetworkU k ks o) ->
--     --           case ns of
--     --             NetSIL s ns' ->
--     --               case ds of
--     --                 DeltasIL _ (delS :: R k) ds' ->
--     --                   let d, s', o :: R k a
--     --                       (d, s') = runRLayerU f_ l x s
--     --                       o = s'
--     --                       deltaos :: Deltas k ks o
--     --                       n'' :: NetworkU k ks o
--     --                       (deltaos, n'') = go nu' o ns' ds'
--     --                       -- deltaos from inputs only, not state
--     --                       deltaos' :: R k
--     --                       deltaos' = case deltaos of            -- yeaa :D
--     --                                    DeltasOL _       -> delS
--     --                                    DeltasIL dos _ _ -> dos ^+^ delS
--     --                       delta :: R k
--     --                       (delta, shft) = unzipV $ liftA2 (adjustHidden (RNode 1 x s)) deltaos' d
--     --                       deltawsI :: R j
--     --                       deltawsS :: R k
--     --                       deltawsI = delta *! (rNodeIWeights <$> ln)
--     --                       deltawsS = delta *! (rNodeSWeights <$> ln)
--     --                   in  (DeltasIL deltawsI deltawsS deltaos, RLayerU shft `NetUIL` n'')
--     --                 _ -> error "impossible.  nu and ds should be same constructors."
--     --             _ -> error "impossible.  nu and ns should be same constructors."
--     -- {-# INLINE trainSample #-}
--     -- trainStates :: forall j hs'. KnownNat j
--     --             => NetworkU j hs' o
--     --             -> NetStates j hs' o
--     --             -> Deltas j hs' o
--     --             -> Network j hs' o
--     -- trainStates nu ns ds =
--     --   case nu of
--     --     NetUOL l -> NetOL l
--     --     NetUIL (RLayerU ln :: RLayerU j k) (nu' :: NetworkU k ks o) ->
--     --       case ns of
--     --         NetSIL s ns' ->
--     --           case ds of
--     --             DeltasIL _ (delS :: R k) ds' ->
--     --               let s' = liftA2 (\d s0 -> s0 - d * stepS) delS s
--     --               in  RLayer ln s' `NetIL` trainStates nu' ns' ds'
--     --             _ -> error "impossible.  nu and ds should be same constructors."
--     --         _ -> error "impossible.  nu and ns should be same constructors."
--     -- {-# INLINE trainStates #-}
--     -- adjustOutput :: KnownNat j => Node j a -> a -> a -> (a, Node j a)
--     -- adjustOutput xb y' d = (delta, weightShifts delta xb)
--     --   where
--     --     delta = let (o, o') = diff' g d
--     --             in  (o - y') * o'
--     -- {-# INLINE adjustOutput #-}
--     -- adjustHidden :: KnownNat k => RNode j k a -> a -> a -> (a, RNode j k a)
--     -- adjustHidden xb deltao d = (delta, weightShifts delta xb)
--     --   where
--     --     -- instead of (o - target), use deltao, weighted average of errors
--     --     delta = deltao * diff f d
--     -- {-# INLINE adjustHidden #-}
--     -- weightShifts :: Functor f => a -> f a -> f a
--     -- weightShifts delta = fmap (\x -> delta * x)
--     -- {-# INLINE weightShifts #-}
