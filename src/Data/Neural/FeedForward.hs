{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Data.Neural.FeedForward where

-- import Control.Applicative
-- import Control.DeepSeq
-- import Control.Monad
-- import Control.Monad.ST
-- import Data.Neural.Types
-- import Control.Monad.Trans.State
-- import Data.Bifunctor
-- import Data.Neural.Utility
-- import Data.Foldable
-- import Data.List
-- import Data.Monoid
-- import Data.Proxy
-- import GHC.Generics
-- import GHC.TypeLits
-- import Linear
-- import Linear.V
-- import Numeric.AD.Rank1.Forward
-- import System.Random
-- import Text.Printf
-- import qualified Control.Lens        as L
-- import qualified Data.Binary         as B
-- import qualified Data.List           as P
-- import qualified Data.Vector         as V
-- import qualified Data.Vector.Mutable as VM

-- data Network :: Nat -> [Nat] -> Nat -> * -> * where
--     OLayer :: !(Layer i o a) -> Network i '[] o a
--     ILayer :: KnownNat j => !(Layer i j a) -> !(Network j hs o a) -> Network i (j ': hs) o a

-- infixr 5 `ILayer`

-- deriving instance Functor (Network i hs o)

-- -- is it important to inline?  think about it later maybe.
-- -- instance Functor (Network i hs o) where
-- --     fmap f n = case n of
-- --                  OLayer l -> OLayer (fmap f l)
-- --                  ILayer l n' -> fmap f l `ILayer` fmap f n'
-- --     {-# INLINE fmap #-}

-- instance (KnownNat i, KnownNat o) => Applicative (Network i '[] o) where
--     pure = OLayer . pure
--     {-# INLINE pure #-}
--     OLayer f <*> OLayer x = OLayer (f <*> x)
--     {-# INLINE (<*>) #-}

-- instance (KnownNat i, KnownNat o, KnownNat j, Applicative (Network j hs o)) => Applicative (Network i (j ': hs) o) where
--     pure x = pure x `ILayer` pure x
--     {-# INLINE pure #-}
--     ILayer fi fr <*> ILayer xi xr = ILayer (fi <*> xi) (fr <*> xr)
--     {-# INLINE (<*>) #-}

-- instance (KnownNat i, KnownNat o, Random a) => Random (Network i '[] o a) where
--     random = first OLayer . random
--     randomR (OLayer rmn, OLayer rmx) = first OLayer . randomR (rmn, rmx)

-- instance (KnownNat i, KnownNat o, KnownNat j, Random a, Random (Network j hs o a)) => Random (Network i (j ': hs) o a) where
--     random g = let (l, g') = random g
--                in  first (l `ILayer`) (random g')
--     randomR (ILayer lmn nmn, ILayer lmx nmx) g =
--         let (l , g') = randomR (lmn, lmx) g
--         in  first (l `ILayer`) (randomR (nmn, nmx) g')

-- instance (KnownNat i, KnownNat o, B.Binary a) => B.Binary (Network i '[] o a) where
--     put (OLayer l) = B.put l
--     get = OLayer <$> B.get

-- instance (KnownNat i, KnownNat o, KnownNat j, B.Binary a, B.Binary (Network j hs o a)) => B.Binary (Network i (j ': hs) o a) where
--     put (ILayer l n') = B.put l *> B.put n'
--     get = ILayer <$> B.get <*> B.get

-- instance NFData a => NFData (Network i hs o a) where
--     rnf (OLayer (force -> !l)) = ()
--     rnf (ILayer (force -> !l) (force -> !n)) = ()

-- deriving instance Show a => Show (Network i hs o a)
-- deriving instance Foldable (Network i hs o)
-- deriving instance Traversable (Network i hs o)

-- runNetwork :: forall i hs o a. (KnownNat i, Num a) => (a -> a) -> (a -> a) -> Network i hs o a -> V i a -> V o a
-- runNetwork f g = go
--   where
--     go :: forall i' hs' o'. KnownNat i' => Network i' hs' o' a -> V i' a -> V o' a
--     go n v = case n of
--                OLayer l    -> g <$> runLayer l v
--                -- OLayer l    ->                        runLayer l v
--                ILayer l n' -> go n' (f <$> runLayer l v)
-- {-# INLINE runNetwork #-}


-- trainSample :: forall i o a hs. (KnownNat i, KnownNat o, Num a)
--             => a -> (Forward a -> Forward a) -> (Forward a -> Forward a)
--             -> V i a -> V o a
--             -> Network i hs o a
--             -> Network i hs o a
-- trainSample step f g x y n = snd $ go x n
--   where
--     -- x: input
--     -- y: target
--     -- d: x * w
--     -- o: f d
--     go :: forall j hs'. KnownNat j => V j a -> Network j hs' o a -> (V j a, Network j hs' o a)
--     go x n =
--       case n of
--         OLayer l@(Layer ln)    ->
--           let d              :: V o a
--               d              = runLayer l x
--               delta          :: V o a
--               ln'            :: V o (Node j a)
--               (delta, ln')   = unzipV $ liftA3 (adjustOutput xb) ln y d
--               -- drop contrib from bias term
--               deltaws        :: V j a
--               deltaws        = delta *! (nodeWeights <$> ln')
--               l'             :: Layer j o a
--               l'             = Layer ln'
--           in  (deltaws, OLayer l')
--         ILayer l@(Layer ln) n' ->
--           let d                    = runLayer l x
--               o                    = fst . diff' f <$> d
--               (deltaos, n'')       = go o n'
--               (delta, ln')         = unzipV $ liftA3 (adjustHidden xb) ln deltaos d
--               deltaws              = delta *! (nodeWeights <$> ln')
--               l'                   = Layer ln'
--           in  (deltaws, l' `ILayer` n'')
--       where
--         xb = Node 1 x
--     {-# INLINE go #-}
--     -- per neuron/node traversal
--     -- every neuron has a delta
--     adjustOutput :: KnownNat j => Node j a -> Node j a -> a -> a -> (a, Node j a)
--     adjustOutput xb node y d = (delta, adjustWeights delta xb node)
--       where
--         delta = let (o, o') = diff' g d
--                 in  (o - y) * o'
--         -- delta = (f d - y) * f' d
--     {-# INLINE adjustOutput #-}
--         -- delta = d - y
--     adjustHidden :: KnownNat j => Node j a -> Node j a -> a -> a -> (a, Node j a)
--     adjustHidden xb node deltao d = (delta, adjustWeights delta xb node)
--       where
--         delta = deltao * diff f d
--     {-# INLINE adjustHidden #-}
--         -- delta = deltao
--     -- per weight traversal
--     adjustWeights :: KnownNat j => a -> Node j a -> Node j a -> Node j a
--     adjustWeights delta = liftA2 (\x -> subtract (step * delta * x))
--     {-# INLINE adjustWeights #-}
-- {-# INLINE trainSample #-}

-- -- logistic' :: Double -> Double
-- -- logistic' = liftA2 (*) logistic (\x -> 1 - logistic x)
-- -- {-# INLINE logistic' #-}

-- networkHeatmap :: (KnownNat i, Num a) => (a -> a) -> (a -> a) -> Network i hs o a -> V i a -> [[a]]
-- networkHeatmap f g n v =
--     vToList v : case n of
--       OLayer l    -> [vToList (g <$> runLayer l v)]
--       ILayer l n' -> networkHeatmap f g n' $ f <$> runLayer l v
--   where
--     vToList = V.toList . toVector

-- drawHeatmap :: KnownNat i => (Double -> Double) -> (Double -> Double) -> Network i hs o Double -> V i Double -> String
-- drawHeatmap f g n = unlines
--                   . map (intercalate "\t")
--                   . P.transpose
--                   . map (padLists ' ')
--                   . padLists ""
--                   . map (padLists ' ' . map (printf "% .3f"))
--                   . networkHeatmap f g n
--   where
--     padLists :: forall a. a -> [[a]] -> [[a]]
--     padLists p xss = flip map xss $ \xs ->
--                        let d = (maxlen - length xs) `div` 2
--                        in  take maxlen $ replicate d p ++ xs ++ repeat p
--       where
--         maxlen = maximum (map length xss)

-- drawNetwork :: forall i hs o. Dim i => Network i hs o Double -> String
-- drawNetwork = unlines
--             . map (intercalate "\t")
--             . P.transpose
--             . map (padLists ' ')
--             . padLists ""
--             . map (intercalate [""])
--             . doublePad ""
--             . ([]:)
--             . (replicate (reflectDim (Proxy :: Proxy i)) ["o"] :)
--             . addDot
--             . (map . map . map) (printf "% .3f")
--             . networkToList
--   where
--     addDot :: [[[String]]] -> [[[String]]]
--     addDot = concatMap $ \xs -> [xs, replicate (length xs) ["o"]]
--     bracketize :: String -> String
--     bracketize str = '[' : str ++ "]"
--     padLists :: forall a. a -> [[a]] -> [[a]]
--     padLists p xss = flip map xss $ \xs ->
--                        let d = (maxlen - length xs) `div` 2
--                        in  take maxlen $ replicate d p ++ xs ++ repeat p
--       where
--         maxlen = maximum (map length xss)
--     doublePad :: forall a. a -> [[[a]]] -> [[[a]]]
--     doublePad p xsss = flip (map . map) xsss $ \xs ->
--                          let d = (maxlen - length xs) `div` 2
--                          in  take maxlen $ replicate d p ++ xs ++ repeat p
--       where
--         maxlen = maximum (concatMap (map length) xsss)
--     nodeToList :: forall j a. Node j a -> [a]
--     nodeToList (Node b (V w)) = b : V.toList w
--     layerToList :: forall i o a. Layer i o a -> [[a]]
--     layerToList (Layer (V l)) = nodeToList <$> V.toList l
--     networkToList :: forall i hs o a. Network i hs o a -> [[[a]]]
--     networkToList n' = case n' of
--                          OLayer l     -> [layerToList l]
--                          ILayer l n'' -> layerToList l : networkToList n''

-- randomNetwork :: (RandomGen g, Random (Network i hs o a), Num a)
--               => g
--               -> (Network i hs o a, g)
-- randomNetwork g = (first . fmap) (subtract 1 . (*2)) $ random g

-- randomNetworkIO :: (Random (Network i hs o a), Num a) => IO (Network i hs o a)
-- randomNetworkIO = fmap (subtract 1 . (*2)) <$> randomIO

-- networkStructure :: forall i hs o a. (KnownNat i, KnownNat o) => Network i hs o a -> (Int, [Int], Int)
-- networkStructure (OLayer l) = (reflectDim (Proxy :: Proxy i), [], reflectDim (Proxy :: Proxy o))
-- networkStructure (ILayer l n') = (reflectDim (Proxy :: Proxy i), j : hs, o)
--   where
--     (j, hs, o) = networkStructure n'

-- -- induceOutput :: forall i hs o a. (KnownNat i, KnownNat o, Floating a, Ord a) => a -> a -> (a, a) -> (a -> a) -> Network i hs o a -> V o a -> V i a -> V i a
-- -- induceOutput nudge step (mn,mx) f n y x0@(V x0v) = V . fst $ foldl' g (x0v, errFrom x0) [0..V.length x0v - 1]
-- --   where
-- --     errFrom = qd y . runNetwork f n
-- --     g (x, e) i = let x'  = V.modify (\v -> VM.write v i . clamp . (+ nudge) =<< VM.read v i) x
-- --                      e'  = errFrom (V x')
-- --                      x'' = V.modify (\v -> VM.write v i . clamp . subtract (nudge*step/e') =<< VM.read v i) x
-- --                      e'' = errFrom (V x'')
-- --                  in  (x'', e'')
-- --     clamp = min mx . max mn
