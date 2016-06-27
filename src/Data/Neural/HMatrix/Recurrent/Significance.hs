{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Data.Neural.HMatrix.Recurrent.Significance
  ( significances
  )
  where

import Data.List.NonEmpty
import Data.Neural.Activation
import Data.Neural.HMatrix.Recurrent
import Data.Neural.HMatrix.Recurrent.Train
import Data.Neural.HMatrix.FLayer
import Data.Neural.Types                   (KnownNet)
import GHC.TypeLits
import GHC.TypeLits.List
import Numeric.AD.Rank1.Forward
import Numeric.LinearAlgebra.Static

data NetSigs' :: Nat -> [Nat] -> Nat -> * where
    NetSiOL :: !(R o)
            -> NetSigs' i '[] o
    NetSiIL :: (KnownNat j, KnownNats js)
            => !(L o j)
            -> !(L j j)
            -> !(NetSigs' j js o)
            -> NetSigs' i (j ': js) o

infixr 5 `NetSiIL`

data NetSigs :: Nat -> [Nat] -> Nat -> * where
    (:#) :: !(L o i)
         -> !(NetSigs' i hs o)
         -> NetSigs i hs o

infixr 4 :#

significances :: forall i hs o. (KnownNet i hs o)
              => NeuralActs (Forward Double)
              -> Network i hs o
              -> NonEmpty (R i)
              -> NetSigs i hs o
significances (NA f g) n0 (x0 :| xs0) = undefined
  where
    na' = NA (fst . diff' f) (fst . diff' g)
    (ns0, nu0) = toNetworkU n0
    sigFinal :: NetStates i hs o -> R i -> NetSigs i hs o
    sigFinal ns x = go nu0 ns x 1
      where
        go :: forall j js. KnownNat j => NetworkU j js o -> NetStates j js o -> R j -> R j -> NetSigs j js o
        go nu ns fx f'x =
          case nu of
            NetUOL l@(FLayer b w) ->
              let z    = runFLayer l fx
                  dRdz = dvmap (diff g) z
                  -- dR'dx = f'x * w
              in  undefined

    -- sigFinal :: NetStates i hs o a -> V i a -> NetSigs i hs o a
    -- sigFinal ns x = go nu0 ns x 1
    --   where
    --     go :: forall j js. KnownNat j => NetworkU j js o a -> NetStates j js o a -> V j a -> V j a -> NetSigs j js o a
    --     go nu ns fx f'x =
    --       case nu of
    --         NetUOL l@(FLayer ln) ->
    --           let z = runFLayer l fx
    --               dRdz = diff g <$> z -- result *is* R
    --               dR'dx = liftA2 (*^) dRdz (liftA2 (*) f'x . nodeWeights <$> ln)
    --           in  dR'dx :# NetSiOL dRdz
    --         NetUIL (RLayerU ln) nu' ->
    --           case ns of
    --             NetSIL s ns' ->
    --               let z = ln !* RNode 1 fx s
    --                   (fz, f'z) = unzipV $ diff' f <$> z
    --                   dR'dz :# sg = go nu' ns' fz f'z
    --                   dRdz = dR'dz    -- no contribution from downstream state
    --                   dSdz = scaled f'z -- result *is* S
    --                   dR'dx = dRdz !*! (liftA2 (*) f'x . rNodeIWeights <$> ln)
    --               in  dR'dx :# NetSiIL dRdz dSdz sg
    -- sigSample :: NetStates i hs o a -> NetSigs i hs o a -> V i a -> NetSigs i hs o a
    -- sigSample = undefined
    --   where
    --     go :: forall j js (k :: Nat) (ks :: [Nat]). KnownNat j
    --        => NetworkU j js o a
    --        -> NetStates j js o a
    --        -> NetSigs' j js o a
    --        -> V j a -> V j a
    --        -> NetSigs j js o a
    --     go nu ns ng fx f'x =
    --       case nu of
    --         NetUOL l@(FLayer ln) ->
    --           let z = runFLayer l fx
    --               dRdz = diff g <$> z
    --               dR'dx = liftA2 (*^) dRdz (liftA2 (*) f'x . nodeWeights <$> ln)
    --           in  dR'dx :# NetSiOL dRdz
    --         -- NetUIL ((RLayerU ln) :: RLayerU j k a) (nu' :: NetworkU k ks o a) ->
    --         NetUIL (RLayerU ln) nu' ->
    --           case ns of
    --             NetSIL s ns' ->
    --               case ng of
    --                 -- NetSiIL (dRdz' :: V o (V k a)) (dSdz' :: V k (V k a)) (ng' :: NetSigs' k ks o a) ->
    --                 NetSiIL dRdz' dSdz' ng' ->
    --                   let z = ln !* RNode 1 fx s
    --                       (fz, f'z) = unzipV $ diff' f <$> z
    --                       dR'dz :# sg = go nu' ns' ng' fz f'z
    --                       -- -- dRdz = dR'dz ^+^ (dRdz' !*! (_ $ rNodeIWeights <$> ln))
    --                       -- dRdz = dR'dz ^+^ (dRdz' !*! undefined)
    --                   -- in  undefined :# NetSiIL dRdz undefined sg
    --                   in  undefined :# NetSiIL undefined undefined sg


