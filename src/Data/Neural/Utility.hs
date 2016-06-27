{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Data.Neural.Utility where

import           Control.DeepSeq
import           Data.Bifunctor
import           Data.Finite
import           Data.Foldable
import           Data.Maybe                   (fromJust)
import           Data.Neural.Types
import           Data.Proxy
import           Data.Reflection
import           GHC.TypeLits
import           GHC.TypeLits.List
import           Linear
import           Linear.V
import qualified Data.Vector                  as V
import qualified Data.Vector.Generic          as VG
import qualified Numeric.LinearAlgebra.Static as H

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


vToR :: KnownNat n => V n Double -> H.R n
vToR (V v) = fromJust $ H.create (VG.convert v)


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

netStructVal :: Integer -> [Integer] -> Integer -> Maybe NetStruct
netStructVal x ys z = do
    SomeNat  i  <- someNatVal x
    SomeNats hs <- someNatsVal ys
    SomeNat  o  <- someNatVal z
    return $ NetStruct i hs o

reifyNetStruct :: Integer -> [Integer] -> Integer
               -> (forall i hs o. (KnownNat i, KnownNat o, KnownNats hs)
                               => Proxy i -> NatList hs -> Proxy o -> r )
               -> r
reifyNetStruct x ys z f = reifyNat x $ \i ->
                            reifyNats ys $ \hs ->
                              reifyNat z $ \o ->
                                f i hs o

fromNetStruct :: NetStruct -> (Integer, [Integer], Integer)
fromNetStruct ns = case ns of
                     NetStruct i hs o -> (natVal i, natsVal hs, natVal o)

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

deleteV :: Finite n -> V n a -> V (n - 1) a
deleteV fn (V v) = V $ deleteVec (fromIntegral (getFinite fn)) v

snocV :: V n a -> a -> V (n + 1) a
snocV (V v) x = V $ V.snoc v x

deleteVec :: Int -> V.Vector a -> V.Vector a
deleteVec i v = let (v0, v1) = V.splitAt i v
                in  v0 V.++ V.tail v1

processSeries
    :: forall a b n. KnownNat n
    => V.Vector (a, b)
    -> V.Vector (V n a, b)
processSeries ios = fmap (bimap (V . V.take n) (V.! n) . V.unzip)
                  . foldl' (V.zipWith V.snoc) (V.replicate k V.empty)
                  . V.iterateN (n+1) (V.drop 1)
                  $ ios
  where
    n = fromInteger $ natVal (Proxy :: Proxy n)
    m = V.length ios
    k = m - n + 1

