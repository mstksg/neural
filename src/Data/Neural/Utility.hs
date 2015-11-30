{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Data.Neural.Utility where

import qualified Data.Vector         as V
import GHC.TypeLits
import Data.Proxy
import Control.DeepSeq
import Unsafe.Coerce
import Data.Neural.Types
import Linear.V
import Linear
import Type.Class.Witness

unzipV :: V i (a, b) -> (V i a, V i b)
unzipV (V v) = (V x, V y)
  where
    (x, y) = V.unzip v
{-# INLINE unzipV #-}

unzipV3 :: V i (a, b, c) -> (V i a, V i b, V i c)
unzipV3 (V v) = (V x, V y, V z)
  where
    (x, y, z) = V.unzip3 v
{-# INLINE unzipV3 #-}


logistic :: Floating a => a -> a
logistic = recip . (+ 1) . exp . negate
{-# INLINE logistic #-}

naLogId :: Floating a => NeuralActs a
naLogId = NA logistic id

naLogLog :: Floating a => NeuralActs a
naLogLog = NA logistic logistic


runFLayer :: (KnownNat i, Num a) => FLayer i o a -> V i a -> V o a
runFLayer (FLayer l) v = l !* Node 1 v
{-# INLINE runFLayer #-}

foldl'' :: NFData b => (b -> a -> b) -> b -> [a] -> b
foldl'' f = go
  where
    go z []     = z
    go z (x:xs) = let y = f z x in y `deepseq` go y xs


-- data Pair5 :: (j -> k -> l -> m -> *) -> (j -> k -> l -> m -> *) -> j -> k -> l -> m -> * where
--     P5 :: f a b c d -> g a b c d -> Pair5 f g a b c d


-- -- newtype MagicNatAdd r = MagicNatAdd (forall (n :: Nat) (m :: Nat). KnownNat (n + m) => Proxy n -> Proxy m -> r)
-- newtype MagicNatAdd r = MagicNatAdd (forall (n :: Nat) (m :: Nat). KnownNat (n + m) => Proxy n -> Proxy m -> r)

-- reifyAddition :: forall n m a. (KnownNat n, KnownNat m) => Proxy n -> Proxy m -> (forall n' m'. KnownNat (n' + m') => Proxy n' -> Proxy m' -> a) -> a
-- reifyAddition x y f = unsafeCoerce (MagicNatAdd f :: MagicNatAdd a) (natVal x + natVal y) Proxy Proxy

-- http://lpaste.net/145826
knWit :: KnownNat n => Proxy n -> Wit (KnownNat n)
knWit _ = Wit

witVal :: forall n. Wit (KnownNat n) -> Integer
witVal Wit = natVal (Proxy :: Proxy n)

infixl 6 %+
(%+) :: forall n m. Wit (KnownNat n) -> Wit (KnownNat m) -> Wit (KnownNat (n + m))
x %+ y = case someNatVal (witVal x + witVal y) of
           Just (SomeNat z) -> unsafeCoerce (knWit z)
           _                -> error "what"

infixl 7 %*
(%*) :: forall n m. Wit (KnownNat n) -> Wit (KnownNat m) -> Wit (KnownNat (n * m))
x %* y = case someNatVal (witVal x * witVal y) of
           Just (SomeNat z) -> unsafeCoerce (knWit z)
           _                -> error "what"


withWit :: Wit c -> (c => r) -> r
withWit w x = case w of Wit -> x

natAddition :: (KnownNat n, KnownNat m) => Proxy n -> Proxy m -> Wit (KnownNat (n + m))
natAddition x y = knWit x %+ knWit y


-- test :: forall n m r. Wit (KnownNat n) -> Wit (KnownNat m) -> (KnownNat (n + m) => r) -> r
-- test x y f = case x %+ y of
--                Wit -> f

-- logistic' :: Floating a => a -> a
-- logistic' = diff logistic
-- {-# INLINE logistic' #-}

iterateN :: forall a. NFData a => (a -> a) -> a -> Int -> a
iterateN f = go
  where
    go :: a -> Int -> a
    go x n = if n <= 0
               then x
               else let x' = f x
                    in x' `deepseq` go x' (n - 1)
{-# INLINE iterateN #-}

class Nudges w where
    nudges :: (a -> a) -> w a -> w (w a)

instance Dim n => Nudges (V n) where
    nudges f v = V . V.generate (dim v) $ \i -> accumV (\x _ -> f x) v [(i, ())]

instance Dim n => Nudges (Node n) where
    nudges f (Node b w) = Node (Node (f b) w) (Node b <$> nudges f w)

instance (Dim i, Dim o) => Nudges (FLayer i o) where
    -- could this be written using the nudges from the other things?
    nudges f (FLayer l) = FLayer . (fmap.fmap) FLayer
                        . V . V.generate (dim l) $ \i ->
                            Node (accumV (\(Node b w) _ -> Node (f b) w) l [(i,())])
                          . V . V.generate dimI $ \j ->
                                  (accumV (\(Node b w) _ -> Node b (accumV (\x _ -> f x) w [(j,())])) l [(i,())])
      where
        dimI = reflectDim (Proxy :: Proxy i)

accumV :: (a -> b -> a) -> V n a -> [(Int, b)] -> V n a
accumV f (V v) xs = V (V.accum f v xs)

