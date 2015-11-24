{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Data.Neural.Utility where

-- import qualified Data.Vector         as V
-- import GHC.TypeLits
-- import Data.Proxy
-- import Unsafe.Coerce
-- import Data.Neural.Types
-- import Linear.V
-- import Linear
-- import Numeric.AD
-- import Type.Class.Witness

-- unzipV :: V i (a, b) -> (V i a, V i b)
-- unzipV (V v) = (V x, V y)
--   where
--     (x, y) = V.unzip v
-- {-# INLINE unzipV #-}

-- logistic :: Floating a => a -> a
-- logistic = recip . (+ 1) . exp . negate
-- {-# INLINE logistic #-}

-- runLayer :: (KnownNat i, Num a) => Layer i o a -> V i a -> V o a
-- runLayer (Layer l) v = l !* Node 1 v
-- {-# INLINE runLayer #-}

-- -- -- newtype MagicNatAdd r = MagicNatAdd (forall (n :: Nat) (m :: Nat). KnownNat (n + m) => Proxy n -> Proxy m -> r)
-- -- newtype MagicNatAdd r = MagicNatAdd (forall (n :: Nat) (m :: Nat). KnownNat (n + m) => Proxy n -> Proxy m -> r)

-- -- reifyAddition :: forall n m a. (KnownNat n, KnownNat m) => Proxy n -> Proxy m -> (forall n' m'. KnownNat (n' + m') => Proxy n' -> Proxy m' -> a) -> a
-- -- reifyAddition x y f = unsafeCoerce (MagicNatAdd f :: MagicNatAdd a) (natVal x + natVal y) Proxy Proxy

-- -- http://lpaste.net/145826
-- knWit :: KnownNat n => Proxy n -> Wit (KnownNat n)
-- knWit _ = Wit

-- witVal :: forall n. Wit (KnownNat n) -> Integer
-- witVal Wit = natVal (Proxy :: Proxy n)

-- (%+) :: forall n m. Wit (KnownNat n) -> Wit (KnownNat m) -> Wit (KnownNat (n + m))
-- x %+ y = case someNatVal (witVal x + witVal y) of
--            Just (SomeNat z) -> unsafeCoerce (knWit z)
--            _                -> error "what"

-- withWit :: Wit c -> (c => r) -> r
-- withWit w x = case w of Wit -> x

-- natAddition :: (KnownNat n, KnownNat m) => Proxy n -> Proxy m -> Wit (KnownNat (n + m))
-- natAddition x y = knWit x %+ knWit y


-- -- test :: forall n m r. Wit (KnownNat n) -> Wit (KnownNat m) -> (KnownNat (n + m) => r) -> r
-- -- test x y f = case x %+ y of
-- --                Wit -> f


-- logistic' :: Floating a => a -> a
-- logistic' = diff logistic
-- {-# INLINE logistic' #-}

