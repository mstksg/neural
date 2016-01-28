{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}


module Data.Neural.Recurrent.Significance where

import Data.List.NonEmpty hiding (transpose)
import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Data.Neural.Recurrent
import Data.Neural.Recurrent.Train
import Data.Neural.Types
import Data.Neural.Utility
import Data.Functor.Identity
import GHC.TypeLits
import GHC.TypeLits.List
import Linear
import Linear.V
import Numeric.AD.Rank1.Forward

data NetSigs' :: Nat -> [Nat] -> Nat -> * -> * where
    NetSiOL :: !(V o a)
            -> NetSigs' i '[] o a
    NetSiIL :: (KnownNat j, KnownNats js)
            => !(V o (V j a))
            -> !(V j (V j a))
            -> !(NetSigs' j js o a)
            -> NetSigs' i (j ': js) o a

data NetSigs :: Nat -> [Nat] -> Nat -> * -> * where
    (:#) :: !(V o (V i a))
         -> !(NetSigs' i hs o a)
         -> NetSigs i hs o a

-- significances :: forall i hs o a. (Floating a, KnownNet i hs o, NFData a)
--               => NeuralActs (Forward a)
--               -> Network i hs o a
--               -> NonEmpty (V i a)
--               -> NetSigs i hs o a
-- significances (NA f g) n0 (x0 :| xs0) = undefined
--   where
--     na' = NA (fst . diff' f) (fst . diff' g)
--     (ns0, nu0) = toNetworkU n0
--     sigFinal :: NetStates i hs o a -> V i a -> NetSigs i hs o a
--     sigFinal ns x = go nu0 ns x 1
--       where
--         go :: forall j js. KnownNat j => NetworkU j js o a -> NetStates j js o a -> V j a -> V j a -> NetSigs j js o a
--         go nu ns fx f'x =
--           case nu of
--             NetUOL l@(FLayer ln) ->
--               let z = runFLayer l fx
--                   dRdz = diff g <$> z -- result *is* R
--                   dR'dx = liftA2 (*^) dRdz (liftA2 (*) f'x . nodeWeights <$> ln)
--               in  dR'dx :# NetSiOL dRdz
--             NetUIL (RLayerU ln) nu' ->
--               case ns of
--                 NetSIL s ns' ->
--                   let z = ln !* RNode 1 fx s
--                       (fz, f'z) = unzipV $ diff' f <$> z
--                       dR'dz :# sg = go nu' ns' fz f'z
--                       dRdz = dR'dz    -- no contribution from downstream state
--                       dSdz = scaled f'z -- result *is* S
--                       dR'dx = dRdz !*! (liftA2 (*) f'x . rNodeIWeights <$> ln)
--                   in  dR'dx :# NetSiIL dRdz dSdz sg
--     sigSample :: NetStates i hs o a -> NetSigs i hs o a -> V i a -> NetSigs i hs o a
--     sigSample = undefined
--       where
--         go :: forall j js (k :: Nat) (ks :: [Nat]). KnownNat j
--            => NetworkU j js o a
--            -> NetStates j js o a
--            -> NetSigs' j js o a
--            -> V j a -> V j a
--            -> NetSigs j js o a
--         go nu ns ng fx f'x =
--           case nu of
--             NetUOL l@(FLayer ln) ->
--               let z = runFLayer l fx
--                   dRdz = diff g <$> z
--                   dR'dx = liftA2 (*^) dRdz (liftA2 (*) f'x . nodeWeights <$> ln)
--               in  dR'dx :# NetSiOL dRdz
--             -- NetUIL ((RLayerU ln) :: RLayerU j k a) (nu' :: NetworkU k ks o a) ->
--             NetUIL (RLayerU ln) nu' ->
--               case ns of
--                 NetSIL s ns' ->
--                   case ng of
--                     -- NetSiIL (dRdz' :: V o (V k a)) (dSdz' :: V k (V k a)) (ng' :: NetSigs' k ks o a) ->
--                     NetSiIL dRdz' dSdz' ng' ->
--                       let z = ln !* RNode 1 fx s
--                           (fz, f'z) = unzipV $ diff' f <$> z
--                           dR'dz :# sg = go nu' ns' ng' fz f'z
--                           -- -- dRdz = dR'dz ^+^ (dRdz' !*! (_ $ rNodeIWeights <$> ln))
--                           -- dRdz = dR'dz ^+^ (dRdz' !*! undefined)
--                       -- in  undefined :# NetSiIL dRdz undefined sg
--                       in  undefined :# NetSiIL undefined undefined sg


--     sigFinal :: NetStates i hs o a -> V i a -> (NetSigsI i hs o a, (V o a, NetStates i hs o a))
-- significances (NA f g) n0 inps0 = case inps0 of

-- data NetSigs :: Nat -> [Nat] -> Nat -> * -> * where
--     NetSiOL :: !(V o a) -> NetSigs i '[] o a
--     NetSiIL :: (KnownNat j, KnownNats js) => !(V j a) -> !(NetSigs j js o a) -> NetSigs i (j ': js) o a

-- data NetSigsI :: Nat -> [Nat] -> Nat -> * -> * where
--     (:#) :: !(V i a) -> !(NetSigs i hs o a) -> NetSigsI i hs o a

-- instance NFData a => NFData (NetSigs i hs o a) where
--     rnf (NetSiOL (force-> !_)) = ()
--     rnf (NetSiIL (force-> !_) (force -> !_)) = ()

-- instance NFData a => NFData (NetSigsI i hs o a) where
--     rnf ((force-> !_) :# (force-> !_)) = ()


-- deriving instance Functor (NetSigs i hs o)
-- deriving instance Foldable (NetSigs i hs o)
-- deriving instance Functor (NetSigsI i hs o)
-- deriving instance Foldable (NetSigsI i hs o)

-- instance KnownNet i hs o => Applicative (NetSigs i hs o) where
--     pure x = case natsList :: NatList hs of
--                ØNL     -> NetSiOL (pure x)
--                _ :<# _ -> pure x `NetSiIL` pure x
--     {-# INLINE pure #-}
--     NetSiOL f     <*> NetSiOL x     = NetSiOL (f <*> x)
--     NetSiIL fi fr <*> NetSiIL xi xr = NetSiIL (fi <*> xi) (fr <*> xr)
--     _             <*> _             = error "this should never happen"
--     {-# INLINE (<*>) #-}

-- instance KnownNet i hs o => Additive (NetSigs i hs o) where
--     zero = pure 0
--     {-# INLINE zero #-}
--     (^+^) = liftA2 (+)
--     {-# INLINE (^+^) #-}
--     (^-^) = liftA2 (-)
--     {-# INLINE (^-^) #-}
--     liftU2 = liftA2
--     {-# INLINE liftU2 #-}
--     liftI2 = liftA2
--     {-# INLINE liftI2 #-}

-- instance KnownNet i hs o => Metric (NetSigs i hs o)

-- instance KnownNet i hs o => Applicative (NetSigsI i hs o) where
--     pure x = pure x :# pure x
--     {-# INLINE pure #-}
--     (f1 :# f2) <*> (x1 :# x2) = (f1 <*> x1) :# (f2 <*> x2)
--     {-# INLINE (<*>) #-}

-- instance KnownNet i hs o => Additive (NetSigsI i hs o) where
--     zero = zero :# zero
--     {-# INLINE zero #-}
--     (i1 :# o1) ^+^ (i2 :# o2) = (i1 ^+^ i2) :# (o1 ^+^ o2)
--     {-# INLINE (^+^) #-}
--     (i1 :# o1) ^-^ (i2 :# o2) = (i1 ^-^ i2) :# (o1 ^-^ o2)
--     {-# INLINE (^-^) #-}
--     lerp a (i1 :# o1) (i2 :# o2) = lerp a i1 i2 :# lerp a o1 o2
--     {-# INLINE lerp #-}
--     liftU2 f (i1 :# o1) (i2 :# o2) = liftU2 f i1 i2 :# liftU2 f o1 o2
--     {-# INLINE liftU2 #-}
--     liftI2 f (i1 :# o1) (i2 :# o2) = liftI2 f i1 i2 :# liftI2 f o1 o2
--     {-# INLINE liftI2 #-}

-- instance KnownNet i hs o => Metric (NetSigsI i hs o) where
--     (i1 :# o1) `dot` (i2 :# o2) = i1 `dot` i2 + o1 `dot` o2
--     {-# INLINE dot #-}
--     quadrance (i :# o) = quadrance i + quadrance o
--     {-# INLINE quadrance #-}
--     (i1 :# o1) `qd` (i2 :# o2) = i1 `qd` i2 + o1 `qd` o2
--     {-# INLINE qd #-}

-- tNetSigsLayers :: forall f i hs o a b. (Applicative f, KnownNat o)
--                => (forall j. KnownNat j => V j a -> f (V j b))
--                -> NetSigs i hs o a
--                -> f (NetSigs i hs o b)
-- tNetSigsLayers f = go
--   where
--     go :: forall j js. NetSigs j js o a -> f (NetSigs j js o b)
--     go n = case n of
--              NetSiOL l -> NetSiOL <$> f l
--              NetSiIL l n' -> NetSiIL <$> f l <*> go n'

-- tNetSigsILayers :: forall f i hs o a b. (Applicative f, KnownNat i, KnownNat o)
--                 => (forall j. KnownNat j => V j a -> f (V j b))
--                 -> NetSigsI i hs o a
--                 -> f (NetSigsI i hs o b)
-- tNetSigsILayers f (i :# o) = (:#) <$> f i <*> tNetSigsLayers f o

-- overNetSigsILayers :: (KnownNat i, KnownNat o)
--                    => (forall j. KnownNat j => V j a -> V j b)
--                    -> NetSigsI i hs o a
--                    -> NetSigsI i hs o b
-- overNetSigsILayers f = runIdentity . tNetSigsILayers (Identity . f)



-- significances :: forall i hs o a. (Floating a, KnownNet i hs o, NFData a)
--               => NeuralActs (Forward a)
--               -> Network i hs o a
--               -> [V i a]
--               -> (NetSigsI i hs o a, ([V o a], NetStates i hs o a))
-- significances (NA f g) n0 inps0 = case inps0 of
--                                    [] -> (pure 0, ([], ns0))
--                                    x:xs -> (first (overNetSigsILayers ((\n -> fmap (/ sum n) n) . (\n -> fmap ((**2) . (/ sum n)) n))))
--                                          $ goTS x ns0 xs
--   where
--     na' = NA (fst . diff' f) (fst . diff' g)
--     (ns0, nu0) = toNetworkU n0
--     goTS :: V i a
--          -> NetStates i hs o a
--          -> [V i a]
--          -> (NetSigsI i hs o a, ([V o a], NetStates i hs o a))
--     goTS x0 (force-> !ns) inps =
--         case inps of
--           []   -> let (force-> !d, (force-> !y, force-> !ns')) = sigFinal ns x0
--                   in  (d, ([y], ns'))
--           x:xs -> let (o, ns') = runNetworkU na' nu0 x0 ns
--                       (force-> !sigs, (os, nst)) = goTS x ns' xs
--                       (force-> !sigs') = sigSample ns x sigs
--                   in  (sigs', (o:os, nst))
--     sigFinal :: NetStates i hs o a -> V i a -> (NetSigsI i hs o a, (V o a, NetStates i hs o a))
--     sigFinal ns1 x0 = go nu0 ns1 x0 (pure 1)
--       where
--         go :: forall j js. KnownNat j
--            => NetworkU j js o a
--            -> NetStates j js o a
--            -> V j a
--            -> V j a
--            -> (NetSigsI j js o a, (V o a, NetStates j js o a))
--         go nu ns fx f'x =
--           case nu of
--             NetUOL l@(FLayer ln) ->
--               let y = runFLayer l fx
--                   -- R = sum of f(y)^2's.
--                   (o, dRdx') = unzipV $ (fst &&& uncurry (*)) . diff' g <$> y
--                   fxf'x = liftI2 (*) fx f'x
--                   dRdx = liftI2 (*) fxf'x $ nodeWeights (dRdx' *! ln)
--               in  (dRdx :# NetSiOL dRdx', (o, NetSOL))
--             NetUIL (RLayerU ln) nu' ->
--               case ns of
--                 NetSIL s ns' ->
--                       -- should contribution from final state be counted?
--                   let y = ln !* RNode 1 fx s
--                       (fx', f'x') = unzipV $ diff' f <$> y
--                       fs = fx'
--                       dSdx' = liftI2 (*) fx' f'x'
--                       (dRdx' :# nsig', (o, s'')) = go nu' ns' fx' f'x'
--                       dRdx = liftI2 (*) f'x $ rNodeIWeights (dRdx' *! ln)
--                   in  (dRdx :# NetSiIL (dRdx' ^+^ dSdx') nsig', (o, NetSIL fs s''))
--                 _ -> error "impossible case"
--     {-# INLINE sigFinal #-}
--     sigSample :: NetStates i hs o a -> V i a -> NetSigsI i hs o a -> NetSigsI i hs o a
--     sigSample ns1 x0 = go nu0 ns1 x0 (pure 1)
--       where
--         go :: forall j js. KnownNat j
--            => NetworkU j js o a
--            -> NetStates j js o a
--            -> V j a
--            -> V j a
--            -> NetSigsI j js o a
--            -> NetSigsI j js o a
--         go nu ns fx f'x (dDdx' :# nsg) =
--           case nu of
--             NetUOL l@(FLayer ln) ->
--               let y = runFLayer l fx
--                   -- R = sum of f(y)^2's.
--                   dRdx' = uncurry (*) . diff' g <$> y
--                   fxf'x = liftI2 (*) fx f'x
--                   dRdx = liftI2 (*) fxf'x $ nodeWeights (dRdx' *! ln)
--               in  dRdx :# NetSiOL dRdx'
--             NetUIL (RLayerU ln) nu' ->
--               case ns of
--                 NetSIL s ns' ->
--                   case nsg of
--                     NetSiIL dSdx' nsg' ->
--                       let y = ln !* RNode 1 fx s
--                           dSdx = liftI2 (*) dSdx' (diff g <$> y) *! (rNodeSWeights <$> ln)
--                           (fx', f'x') = unzipV $ diff' f <$> y
--                           dRdx' :# nsig' = go nu' ns' fx' f'x' (dSdx' :# nsg')
--                           dRdx = liftI2 (*) f'x $ rNodeIWeights (dRdx' *! ln)
--                           -- USAGE OF dDdX' here potentially incorrect
--                           -- MAYBE IT SHOULD BE liftI2 (*) ????
--                       in  (dRdx ^+^ dDdx') :# NetSiIL (dRdx' ^+^ dSdx) nsig'
--                     _ -> error "impossible case."
--                 _ -> error "impossible case."
--     {-# INLINE sigSample #-}
--                       -- is this right?  something feels off.  hm.  why are
--                       -- there two additions to state? oh i see, there is
--                       -- a direct influence, and an indirect one?  uhm.

-- significances_ :: forall i hs o a. (Floating a, KnownNet i hs o, NFData a)
--               => NeuralActs (Forward a)
--               -> Network i hs o a
--               -> [V i a]
--               -> NetSigsI i hs o a
-- significances_ na n xs = fst $ significances na n xs
