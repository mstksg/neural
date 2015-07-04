{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Neural where

import Data.List
import Control.Applicative
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Proxy
import GHC.TypeLits
import Linear
import Linear.V
import System.Random
import Text.Printf
import GHC.Generics
import qualified Data.Binary as B
import qualified Data.List as P
import qualified Data.Vector as V

data Node :: Nat -> * -> * where
    Node :: { nodeBias :: !a, nodeWeights :: !(V i a) } -> Node i a
  deriving (Show, Foldable, Traversable, Generic)
  -- TODO: manually define Foldable, Traversable

newtype Layer :: Nat -> Nat -> * -> * where
    Layer :: { layerNodes :: V o (Node i a) } -> Layer i o a
  deriving (Show, Foldable, Traversable, Functor, Generic)

data Network :: Nat -> [Nat] -> Nat -> * -> * where
    OLayer :: !(Layer i o a) -> Network i '[] o a
    ILayer :: KnownNat j => !(Layer i j a) -> !(Network j hs o a) -> Network i (j ': hs) o a

infixr 5 `ILayer`

instance Functor (V i) => Functor (Node i) where
    fmap f (Node b w) = Node (f b) (fmap f w)
    {-# INLINE fmap #-}

instance Applicative (V i) => Applicative (Node i) where
    pure x = Node x (pure x)
    {-# INLINE pure #-}
    Node fb fw <*> Node xb xw = Node (fb xb) (fw <*> xw)
    {-# INLINE (<*>) #-}

instance (KnownNat i, Additive (V i)) => Additive (Node i) where
    zero = Node 0 zero
    {-# INLINE zero #-}
    Node b1 w1 ^+^ Node b2 w2 = Node (b1 + b1) (w1 ^+^ w2)
    {-# INLINE (^+^) #-}
    Node b1 w1 ^-^ Node b2 w2 = Node (b1 - b1) (w1 ^-^ w2)
    {-# INLINE (^-^) #-}
    lerp a (Node b1 w1) (Node b2 w2) = Node (a * b1 + (1 - a) * b2) (lerp a w1 w2)
    {-# INLINE lerp #-}
    liftU2 f (Node b1 w1) (Node b2 w2) = Node (f b1 b2) (liftU2 f w1 w2)
    {-# INLINE liftU2 #-}
    liftI2 f (Node b1 w1) (Node b2 w2) = Node (f b1 b2) (liftI2 f w1 w2)
    {-# INLINE liftI2 #-}

instance (KnownNat i, KnownNat o) => Applicative (Layer i o) where
    pure = Layer . V . V.replicate (reflectDim (Proxy :: Proxy o)) . pure
    {-# INLINE pure #-}
    Layer f <*> Layer x = Layer (liftA2 (<*>) f x)
    {-# INLINE (<*>) #-}

instance Functor (Network i hs o) where
    fmap f n = case n of
                 OLayer l -> OLayer (fmap f l)
                 ILayer l n' -> fmap f l `ILayer` fmap f n'
    {-# INLINE fmap #-}

instance (KnownNat i, KnownNat o) => Applicative (Network i '[] o) where
    pure = OLayer . pure
    {-# INLINE pure #-}
    OLayer f <*> OLayer x = OLayer (f <*> x)
    {-# INLINE (<*>) #-}

instance (KnownNat i, KnownNat o, KnownNat j, Applicative (Network j hs o)) => Applicative (Network i (j ': hs) o) where
    pure x = pure x `ILayer` pure x
    {-# INLINE pure #-}
    ILayer fi fr <*> ILayer xi xr = ILayer (fi <*> xi) (fr <*> xr)
    {-# INLINE (<*>) #-}

instance (KnownNat i, Random a) => Random (V i a) where
    random g = first V . flip runState g
             $ V.replicateM (reflectDim (Proxy :: Proxy i)) (state random)
    randomR (V rmn, V rmx) g = first V . flip runState g
                             $ V.zipWithM (\x y -> state (randomR (x, y))) rmn rmx

instance (KnownNat i, Random a) => Random (Node i a) where
    random g =
        let (b, g') = random g
        in  first (Node b) (random g')
    randomR (Node bmn wmn, Node bmx wmx) g =
        let (b, g') = randomR (bmn, bmx) g
        in  first (Node b) (randomR (wmn, wmx) g')

instance (KnownNat i, KnownNat o, Random a) => Random (Network i '[] o a) where
    random = first OLayer . random
    randomR (OLayer rmn, OLayer rmx) = first OLayer . randomR (rmn, rmx)

instance (KnownNat i, KnownNat o, KnownNat j, Random a, Random (Network j hs o a)) => Random (Network i (j ': hs) o a) where
    random g = let (l, g') = random g
               in  first (l `ILayer`) (random g')
    randomR (ILayer lmn nmn, ILayer lmx nmx) g =
        let (l , g') = randomR (lmn, lmx) g
        in  first (l `ILayer`) (randomR (nmn, nmx) g')

instance (KnownNat i, B.Binary a) => B.Binary (Node i a)
instance (KnownNat i, KnownNat o, B.Binary a) => B.Binary (Layer i o a)

instance (KnownNat i, KnownNat o, B.Binary a) => B.Binary (Network i '[] o a) where
    put (OLayer l) = B.put l
    get = OLayer <$> B.get

instance (KnownNat i, KnownNat o, KnownNat j, B.Binary a, B.Binary (Network j hs o a)) => B.Binary (Network i (j ': hs) o a) where
    put (ILayer l n') = B.put l *> B.put n'
    get = ILayer <$> B.get <*> B.get


deriving instance (KnownNat i, KnownNat o, Random a) => Random (Layer i o a)
deriving instance Show a => Show (Network i hs o a)
deriving instance Foldable (Network i hs o)
deriving instance Traversable (Network i hs o)

-- liftA4 :: Applicative f
--        => (a -> b -> c -> d -> e)
--        -> f a -> f b -> f c -> f d -> f e
-- liftA4 f j k l m = f <$> j <*> k <*> l <*> m

unzipV :: V i (a, b) -> (V i a, V i b)
unzipV (V v) = (V x, V y)
  where
    (x, y) = V.unzip v
{-# INLINE unzipV #-}

trainSample :: forall i o a hs. (KnownNat i, KnownNat o, Num a)
            => a -> (a -> a) -> (a -> a)
            -> V i a -> V o a
            -> Network i hs o a
            -> Network i hs o a
trainSample step f f' x y n = snd $ go x n
  where
    -- x: input
    -- y: target
    -- d: x * w
    -- o: f d
    go :: forall j hs'. KnownNat j => V j a -> Network j hs' o a -> (V j a, Network j hs' o a)
    go x n =
      case n of
        OLayer l@(Layer ln)    ->
          let d              :: V o a
              d              = runLayer l x
              delta          :: V o a
              ln'            :: V o (Node j a)
              (delta, ln')   = unzipV $ liftA3 (adjustOutput xb) ln y d
              -- drop contrib from bias term
              deltaws        :: V j a
              deltaws        = delta *! (nodeWeights <$> ln')
              l'             :: Layer j o a
              l'             = Layer ln'
          in  (deltaws, OLayer l')
          -- let d              = runLayer l x
          --     (delta, ln')   = unzipV $ liftA3 (adjustOutput xb) ln y d
          --     deltaws        = delta *! (nodeWeights <$> ln')
          --     l'             = Layer ln'
          -- in  (deltaws, OLayer l')
        ILayer l@(Layer ln) n' ->
          let d                    = runLayer l x
              o                    = f <$> d
              (deltaos, n'')       = go o n'
              (delta, ln')         = unzipV $ liftA3 (adjustHidden xb) ln deltaos d
              deltaws              = delta *! (nodeWeights <$> ln')
              l'                   = Layer ln'
          in  (deltaws, l' `ILayer` n'')
      where
        xb = Node 1 x
    {-# INLINE go #-}
    -- per neuron/node traversal
    -- every neuron has a delta
    adjustOutput :: KnownNat j => Node j a -> Node j a -> a -> a -> (a, Node j a)
    adjustOutput xb node y d = (delta, adjustWeights delta xb node)
      where
        delta = (f d - y) * f' d
    {-# INLINE adjustOutput #-}
        -- delta = d - y
    adjustHidden :: KnownNat j => Node j a -> Node j a -> a -> a -> (a, Node j a)
    adjustHidden xb node deltao d = (delta, adjustWeights delta xb node)
      where
        delta = deltao * f' d
    {-# INLINE adjustHidden #-}
        -- delta = deltao
    -- per weight traversal
    adjustWeights :: KnownNat j => a -> Node j a -> Node j a -> Node j a
    adjustWeights delta = liftA2 (\x -> subtract (step * delta * x))
    -- adjustWeight :: a -> a -> a -> a
    -- adjustWeight delta x = subtract $ step * delta * x
    {-# INLINE adjustWeights #-}
{-# INLINE trainSample #-}

runLayer :: (KnownNat i, Num a) => Layer i o a -> V i a -> V o a
runLayer (Layer l) v = l !* Node 1 v
{-# INLINE runLayer #-}

runNetwork :: (KnownNat i, Num a) => (a -> a) -> Network i hs o a -> V i a -> V o a
runNetwork f n v = case n of
                     OLayer l    ->                  f <$> runLayer l v
                     -- OLayer l    ->                        runLayer l v
                     ILayer l n' -> runNetwork f n' (f <$> runLayer l v)
{-# INLINE runNetwork #-}

logistic :: Double -> Double
logistic = recip . (+ 1) . exp . negate
{-# INLINE logistic #-}

logistic' :: Double -> Double
logistic' = liftA2 (*) logistic (\x -> 1 - logistic x)
{-# INLINE logistic' #-}

networkHeatmap :: (KnownNat i, Num a) => (a -> a) -> Network i hs o a -> V i a -> [[a]]
networkHeatmap f n v = vToList v : case n of
                                     OLayer l    -> [vToList (f <$> runLayer l v)]
                                     ILayer l n' -> networkHeatmap f n' $ f <$> runLayer l v
  where
    vToList = V.toList . toVector

drawHeatmap :: KnownNat i => (Double -> Double) -> Network i hs o Double -> V i Double -> String
drawHeatmap f n = unlines
                . map (intercalate "\t")
                . P.transpose
                . map (padLists ' ')
                . padLists ""
                . map (padLists ' ' . map (printf "% .3f"))
                . networkHeatmap f n
  where
    padLists :: forall a. a -> [[a]] -> [[a]]
    padLists p xss = flip map xss $ \xs ->
                       let d = (maxlen - length xs) `div` 2
                       in  take maxlen $ replicate d p ++ xs ++ repeat p
      where
        maxlen = maximum (map length xss)

drawNetwork :: forall i hs o. Dim i => Network i hs o Double -> String
drawNetwork = unlines
            . map (intercalate "\t")
            . P.transpose
            . map (padLists ' ')
            . padLists ""
            . map (intercalate [""])
            . doublePad ""
            . ([]:)
            . (replicate (reflectDim (Proxy :: Proxy i)) ["o"] :)
            . addDot
            . (map . map . map) (printf "% .3f")
            . networkToList
-- drawNetwork = unlines
--             . map unwords
--             . (map . map) (bracketize . unwords)
--             . (map . map . map) (printf "%.2f")
--             . networkToList
  where
    addDot :: [[[String]]] -> [[[String]]]
    addDot = concatMap $ \xs -> [xs, replicate (length xs) ["o"]]
    -- zipUp :: [a] -> [a] -> [a]
    -- zipUp xs ys = concat $ zipWith (\x y -> [x, y]) xs ys
    bracketize :: String -> String
    bracketize str = '[' : str ++ "]"
    padLists :: forall a. a -> [[a]] -> [[a]]
    padLists p xss = flip map xss $ \xs ->
                       let d = (maxlen - length xs) `div` 2
                       in  take maxlen $ replicate d p ++ xs ++ repeat p
      where
        maxlen = maximum (map length xss)
    doublePad :: forall a. a -> [[[a]]] -> [[[a]]]
    doublePad p xsss = flip (map . map) xsss $ \xs ->
                         let d = (maxlen - length xs) `div` 2
                         in  take maxlen $ replicate d p ++ xs ++ repeat p
      where
        maxlen = maximum (concatMap (map length) xsss)
    nodeToList :: forall j a. Node j a -> [a]
    nodeToList (Node b (V w)) = b : V.toList w
    layerToList :: forall i o a. Layer i o a -> [[a]]
    layerToList (Layer (V l)) = nodeToList <$> V.toList l
    networkToList :: forall i hs o a. Network i hs o a -> [[[a]]]
    networkToList n' = case n' of
                         OLayer l     -> [layerToList l]
                         ILayer l n'' -> layerToList l : networkToList n''

randomNetwork :: (RandomGen g, Random (Network i hs o a), Num a)
              => g
              -> (Network i hs o a, g)
randomNetwork g = (first . fmap) (subtract 1 . (*2)) $ random g

randomNetworkIO :: (Random (Network i hs o a), Num a) => IO (Network i hs o a)
randomNetworkIO = fmap (subtract 1 . (*2)) <$> randomIO

networkStructure :: forall i hs o a. (KnownNat i, KnownNat o) => Network i hs o a -> (Int, [Int], Int)
networkStructure (OLayer l) = (reflectDim (Proxy :: Proxy i), [], reflectDim (Proxy :: Proxy o))
networkStructure (ILayer l n') = (reflectDim (Proxy :: Proxy i), j : hs, o)
  where
    (j, hs, o) = networkStructure n'
