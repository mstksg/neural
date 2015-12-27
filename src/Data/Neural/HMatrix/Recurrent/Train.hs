{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Neural.HMatrix.Recurrent.Train where

import Control.DeepSeq
import Data.Neural.HMatrix.Recurrent
import Data.MonoTraversable
import Data.Neural.Types             (NeuralActs(..), KnownNet)
import Control.Monad.Random
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

instance (KnownNat i, KnownNat o) => Num (RLayerU i o) where
    RLayerU b1 wI1 wS1 + RLayerU b2 wI2 wS2 = RLayerU (b1 + b2) (wI1 + wI2) (wS1 + wS2)
    RLayerU b1 wI1 wS1 * RLayerU b2 wI2 wS2 = RLayerU (b1 * b2) (wI1 * wI2) (wS1 * wS2)
    RLayerU b1 wI1 wS1 - RLayerU b2 wI2 wS2 = RLayerU (b1 - b2) (wI1 - wI2) (wS1 - wS2)
    abs (RLayerU b wI wS) = RLayerU (abs b) (abs wI) (abs wS)
    negate (RLayerU b wI wS) = RLayerU (negate b) (negate wI) (negate wS)
    signum (RLayerU b wI wS) = RLayerU (signum b) (signum wI) (signum wS)
    fromInteger = RLayerU <$> fromInteger <*> fromInteger <*> fromInteger

type instance Element (NetworkU i hs o) = Double

-- instance MonoFunctor (NetworkU i hs o) where
--     omap f n = case n of
--                  NetUOL l -> NetUOL (omap f l)

zipNetU :: forall i hs o. KnownNet i hs o
        => (forall j k. (KnownNat j, KnownNat k) => FLayer j k -> FLayer j k -> FLayer j k)
        -> (forall j k. (KnownNat j, KnownNat k) => RLayerU j k -> RLayerU j k -> RLayerU j k)
        -> NetworkU i hs o -> NetworkU i hs o
        -> NetworkU i hs o
zipNetU ff fr = go
  where
    go :: forall j js. KnownNet j js o => NetworkU j js o -> NetworkU j js o -> NetworkU j js o
    go n1 n2 = case n1 of
                 NetUOL l1 ->
                   case n2 of
                     NetUOL l2 -> NetUOL (ff l1 l2)
                     _         -> error "impossible"
                 NetUIL l1 n1' ->
                   case n2 of
                     NetUIL l2 n2' ->
                       NetUIL (fr l1 l2) (go n1' n2')
                     _             ->
                       error "impossible"

mapNetU :: forall i hs o. KnownNet i hs o
        => (forall j k. (KnownNat j, KnownNat k) => FLayer j k -> FLayer j k)
        -> (forall j k. (KnownNat j, KnownNat k) => RLayerU j k -> RLayerU j k)
        -> NetworkU i hs o
        -> NetworkU i hs o
mapNetU ff fr = go
  where
    go :: forall j js. KnownNet j js o => NetworkU j js o -> NetworkU j js o
    go n = case n of
             NetUOL l    -> NetUOL (ff l)
             NetUIL l n' -> NetUIL (fr l) (go n')

pureNetU :: forall i hs o. KnownNet i hs o
         => (forall j k. (KnownNat j, KnownNat k) => FLayer j k)
         -> (forall j k. (KnownNat j, KnownNat k) => RLayerU j k)
         -> NetworkU i hs o
pureNetU lf lr = go
  where
    go :: forall j js. KnownNet j js o => NetworkU j js o
    go = case natsList :: NatList js of
           ØNL     -> NetUOL lf
           _ :<# _ -> lr `NetUIL` go

konstNetU :: KnownNet i hs o
          => Double
          -> NetworkU i hs o
konstNetU i = pureNetU (konstFLayer i) (konstRLayerU i)

instance KnownNet i hs o => Num (NetworkU i hs o) where
    (+) = zipNetU (+) (+)
    (-) = zipNetU (-) (-)
    (*) = zipNetU (*) (*)
    negate = mapNetU negate negate
    abs = mapNetU abs abs
    signum = mapNetU signum signum
    fromInteger i = pureNetU (fromInteger i) (fromInteger i)

konstRLayerU :: (KnownNat i, KnownNat o)
             => Double
             -> RLayerU i o
konstRLayerU = RLayerU <$> konst <*> konst <*> konst

runRLayerU :: (KnownNat i, KnownNat o)
           => (Double -> Double)
           -> RLayerU i o
           -> R i
           -> R o
           -> (R o, R o)
runRLayerU f (RLayerU b wI wS) v s = (v', dvmap f v')
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
                    (dvmap g (runFLayer l v), NetSOL)
                  NetUIL (RLayerU b wI wS) n' ->
                    case ns of
                      NetSIL s ns' ->
                        let v' = dvmap f $ b + wI #> v + wS #> s
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

trainSeries :: forall i hs o. KnownNet i hs o
            => NeuralActs (Forward Double)
            -> Double
            -> Double
            -> R o
            -> [R i]
            -> Network i hs o
            -> Network i hs o
trainSeries (NA f g) step stepS targ inps0 n0 =
    case inps0 of
      [] -> n0
      x0:xs -> let (ds, nuShifts) = goTS x0 ns0 xs
                   nu1 = nu0 - konstNetU step * nuShifts
               in  trainStates nu1 ns0 ds
  where
    na'@(NA f_ g_) = NA (fst . diff' f) (fst . diff' g)
    (ns0, nu0) = toNetworkU n0
    goTS :: R i
         -> NetStates i hs o
         -> [R i]
         -> (Deltas i hs o, NetworkU i hs o)
    goTS (force-> !x) (force-> !s) inps =
        case inps of
          []    -> let (force-> !d, force-> !nu) = trainFinal x s
                   in  (d, nu)
          x':xs ->
            let (_ , s') = runNetworkU na' nu0 x s
                (force-> !d , force-> !nus) = goTS x' s' xs
                -- can "run" values from runNetworkU be re-used here?
                (d', nu) = trainSample x' s d
            in  (d', nu + nus)
    trainFinal :: R i
               -> NetStates i hs o
               -> (Deltas i hs o, NetworkU i hs o)
    trainFinal = go nu0
      where
        go :: forall j js. KnownNat j
           => NetworkU j js o
           -> R j
           -> NetStates j js o
           -> (Deltas j js o, NetworkU j js o)
        go nu x ns =
          case nu of
            NetUOL l@(FLayer _ w) ->
              let y = runFLayer l x
                  o = g_ `dvmap` y
                  dEdy = dvmap (diff g) y * (o - targ)
                  delWs = tr w #> dEdy
                  shiftsW = outer dEdy x
                  shiftsB = dEdy -- should be dEdy * 1
              in  (DeltasOL delWs, NetUOL (FLayer shiftsB shiftsW))
            NetUIL l@(RLayerU _ wI wS :: RLayerU j k) (nu' :: NetworkU k ks o) ->
              case ns of
                NetSIL s ns' ->
                  let (y, s') = runRLayerU f_ l x s
                      o = s' -- o = mapR f_ y
                      (delWs', nu'') = go nu' o ns'
                      delWs'I :: R k
                      delWs'I = case delWs' of
                                  DeltasOL dws -> dws
                                  DeltasIL dws _ _ -> dws
                      dEdy = dvmap (diff f) y * delWs'I
                      delWsI = tr wI #> dEdy
                      delWsS = tr wS #> dEdy
                      shiftsWI = outer dEdy x
                      shiftsWS = outer dEdy s
                      shiftsB = dEdy -- should be dEdy * 1
                  in  (DeltasIL delWsI delWsS delWs', RLayerU shiftsB shiftsWI shiftsWS `NetUIL` nu'')
    trainSample :: R i
                -> NetStates i hs o
                -> Deltas i hs o
                -> (Deltas i hs o, NetworkU i hs o)
    trainSample = go nu0
      where
        go :: forall j js. KnownNat j
           => NetworkU j js o
           -> R j
           -> NetStates j js o
           -> Deltas j js o
           -> (Deltas j js o, NetworkU j js o)
        go nu x ns ds =
          case nu of
            NetUOL _ -> (DeltasOL 0, NetUOL 0)
            NetUIL l@(RLayerU _ wI wS :: RLayerU j k) (nu' :: NetworkU k ks o) ->
              case ns of
                NetSIL s ns' ->
                  case ds of
                    DeltasIL _ (delS :: R k) ds' ->
                      let (y, s') = runRLayerU f_ l x s
                          o = s' -- o = mapR f_ y
                          (delWs', nu'') = go nu' o ns' ds'
                          delWs'I :: R k
                          delWs'I = case delWs' of
                                      DeltasOL _ -> delS
                                      DeltasIL dws _ _ -> dws + delS
                          dEdy = dvmap (diff f) y * delWs'I
                          delWsI = tr wI #> dEdy
                          delWsS = tr wS #> dEdy
                          shiftsWI = outer dEdy x
                          shiftsWS = outer dEdy s
                          shiftsB = dEdy -- should be dEdy * 1
                      in  (DeltasIL delWsI delWsS delWs', RLayerU shiftsB shiftsWI shiftsWS `NetUIL` nu'')
    trainStates :: forall j hs'. KnownNat j
                => NetworkU j hs' o
                -> NetStates j hs' o
                -> Deltas j hs' o
                -> Network j hs' o
    trainStates nu ns ds =
      case nu of
        NetUOL l -> NetOL l
        NetUIL (RLayerU b wI wS :: RLayerU j k) (nu' :: NetworkU k ks o) ->
          case ns of
            NetSIL s ns' ->
              case ds of
                DeltasIL _ (delS :: R k) ds' ->
                  let s' = s - konst stepS * delS
                  in  RLayer b wI wS s' `NetIL` trainStates nu' ns' ds'
                _ -> error "impossible.  nu and ds should be same constructors."
            _ -> error "impossible.  nu and ns should be same constructors."

trainSeriesDO :: forall i hs o m. (KnownNet i hs o, MonadRandom m)
              => NeuralActs (Forward Double)
              -> Double
              -> Double
              -> Double
              -> R o
              -> [R i]
              -> Network i hs o
              -> m (Network i hs o)
trainSeriesDO (NA f g) doRate step stepS targ inps0 n0 = undefined

