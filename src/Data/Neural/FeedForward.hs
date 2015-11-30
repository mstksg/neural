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

import Control.Applicative
import Control.DeepSeq
import Data.Bifunctor
import Data.List
import Data.Neural.Types
import Data.Neural.Utility
import Data.Proxy
import GHC.TypeLits
import Linear
import Linear.V
import Numeric.AD.Rank1.Forward
import System.Random
import Text.Printf
import qualified Data.Binary    as B
import qualified Data.List      as P
import qualified Data.Vector    as V

data Network :: Nat -> [Nat] -> Nat -> *
             -> * where
    NetOL :: !(FLayer i o a) -> Network i '[] o a
    NetIL :: KnownNat j => !(FLayer i j a) -> !(Network j hs o a) -> Network i (j ': hs) o a

infixr 5 `NetIL`


runNetwork :: forall i hs o a. (KnownNat i, Num a) => (a -> a) -> (a -> a) -> Network i hs o a -> V i a -> V o a
runNetwork f g = go
  where
    go :: forall i' hs' o'. KnownNat i' => Network i' hs' o' a -> V i' a -> V o' a
    go n v = case n of
               NetOL l    -> g <$> runFLayer l v
               NetIL l n' -> go n' (f <$> runFLayer l v)
{-# INLINE runNetwork #-}

trainSample :: forall i o a hs. (KnownNat i, KnownNat o, Num a)
            => a -> (Forward a -> Forward a) -> (Forward a -> Forward a)
            -> V i a -> V o a
            -> Network i hs o a
            -> Network i hs o a
trainSample step f g x0 y n0 = snd $ go x0 n0
  where
    -- x: input
    -- y: target
    -- d: x * w
    -- o: f d
    go :: forall j hs'. KnownNat j => V j a -> Network j hs' o a -> (V j a, Network j hs' o a)
    go x n =
      case n of
        NetOL l@(FLayer ln)    ->
          let d              :: V o a
              d              = runFLayer l x
              delta          :: V o a
              ln'            :: V o (Node j a)
              (delta, ln')   = unzipV $ liftA3 (adjustOutput xb) ln y d
              -- drop contrib from bias term
              deltaws        :: V j a
              deltaws        = delta *! (nodeWeights <$> ln')
              l'             :: FLayer j o a
              l'             = FLayer ln'
          in  (deltaws, NetOL l')
        NetIL l@(FLayer ln :: FLayer j k a) (n' :: Network k ks o a) ->
          let d :: V k a
              d                    = runFLayer l x
              o :: V k a
              o                    = fst . diff' f <$> d
              deltaos :: V k a
              n'' :: Network k ks o a
              (deltaos, n'')       = go o n'
              delta :: V k a
              ln' :: V k (Node j a)
              (delta, ln')         = unzipV $ liftA3 (adjustHidden xb) ln deltaos d
              deltaws :: V j a
              deltaws              = delta *! (nodeWeights <$> ln')
              l' :: FLayer j k a
              l'                   = FLayer ln'
          in  (deltaws, l' `NetIL` n'')
      where
        xb = Node 1 x
    -- {-# INLINE go #-}
    -- per neuron/node traversal
    -- every neuron has a delta
    adjustOutput :: KnownNat j => Node j a -> Node j a -> a -> a -> (a, Node j a)
    adjustOutput xb node y' d = (delta, adjustWeights delta xb node)
      where
        delta = let (o, o') = diff' g d
                in  (o - y') * o'
        -- delta = (f d - y) * f' d
    {-# INLINE adjustOutput #-}
        -- delta = d - y
    adjustHidden :: KnownNat j => Node j a -> Node j a -> a -> a -> (a, Node j a)
    adjustHidden xb node deltao d = (delta, adjustWeights delta xb node)
      where
        -- instead of (o - target), use deltao, weighted average of errors
        delta = deltao * diff f d
    {-# INLINE adjustHidden #-}
        -- delta = deltao
    -- per weight traversal
    adjustWeights :: KnownNat j => a -> Node j a -> Node j a -> Node j a
    adjustWeights delta = liftA2 (\w n -> n - step * delta * w)
    {-# INLINE adjustWeights #-}
{-# INLINE trainSample #-}

networkHeatmap :: (KnownNat i, Num a) => (a -> a) -> (a -> a) -> Network i hs o a -> V i a -> [[a]]
networkHeatmap f g n v =
    vToList v : case n of
      NetOL l    -> [vToList (g <$> runFLayer l v)]
      NetIL l n' -> networkHeatmap f g n' $ f <$> runFLayer l v
  where
    vToList = V.toList . toVector

drawHeatmap :: KnownNat i => (Double -> Double) -> (Double -> Double) -> Network i hs o Double -> V i Double -> String
drawHeatmap f g n = unlines
                  . map (intercalate "\t")
                  . P.transpose
                  . map (padLists ' ')
                  . padLists ""
                  . map (padLists ' ' . map (printf "% .3f"))
                  . networkHeatmap f g n
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
  where
    addDot :: [[[String]]] -> [[[String]]]
    addDot = concatMap $ \xs -> [xs, replicate (length xs) ["o"]]
    -- bracketize :: String -> String
    -- bracketize str = '[' : str ++ "]"
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
    layerToList :: forall i' o' a. FLayer i' o' a -> [[a]]
    layerToList (FLayer (V l)) = nodeToList <$> V.toList l
    networkToList :: forall i' hs' o' a. Network i' hs' o' a -> [[[a]]]
    networkToList n' = case n' of
                         NetOL l     -> [layerToList l]
                         NetIL l n'' -> layerToList l : networkToList n''

randomNetwork :: (RandomGen g, Random (Network i hs o a), Num a)
              => g
              -> (Network i hs o a, g)
randomNetwork g = (first . fmap) (subtract 1 . (*2)) $ random g

randomNetworkIO :: (Random (Network i hs o a), Num a) => IO (Network i hs o a)
randomNetworkIO = fmap (subtract 1 . (*2)) <$> randomIO

networkStructure :: forall i hs o a. (KnownNat i, KnownNat o) => Network i hs o a -> (Int, [Int], Int)
networkStructure (NetOL _) = (reflectDim (Proxy :: Proxy i), [], reflectDim (Proxy :: Proxy o))
networkStructure (NetIL _ n') = (reflectDim (Proxy :: Proxy i), j : hs, o)
  where
    (j, hs, o) = networkStructure n'

-- induceOutput :: forall i hs o a. (KnownNat i, KnownNat o, Floating a, Ord a) => a -> a -> (a, a) -> (a -> a) -> Network i hs o a -> V o a -> V i a -> V i a
-- induceOutput nudge step (mn,mx) f n y x0@(V x0v) = V . fst $ foldl' g (x0v, errFrom x0) [0..V.length x0v - 1]
--   where
--     errFrom = qd y . runNetwork f n
--     g (x, e) i = let x'  = V.modify (\v -> VM.write v i . clamp . (+ nudge) =<< VM.read v i) x
--                      e'  = errFrom (V x')
--                      x'' = V.modify (\v -> VM.write v i . clamp . subtract (nudge*step/e') =<< VM.read v i) x
--                      e'' = errFrom (V x'')
--                  in  (x'', e'')
--     clamp = min mx . max mn

-- | Boilerplate instances

instance Functor (Network i hs o) where
    fmap f n = case n of
                 NetOL l -> NetOL (fmap f l)
                 NetIL l n' -> fmap f l `NetIL` fmap f n'
    {-# INLINE fmap #-}

instance (KnownNat i, KnownNat o) => Applicative (Network i '[] o) where
    pure = NetOL . pure
    {-# INLINE pure #-}
    NetOL f <*> NetOL x = NetOL (f <*> x)
    {-# INLINE (<*>) #-}

instance (KnownNat i, KnownNat o, KnownNat j, Applicative (Network j hs o)) => Applicative (Network i (j ': hs) o) where
    pure x = pure x `NetIL` pure x
    {-# INLINE pure #-}
    NetIL fi fr <*> NetIL xi xr = NetIL (fi <*> xi) (fr <*> xr)
    {-# INLINE (<*>) #-}

instance (KnownNat i, KnownNat o, Random a) => Random (Network i '[] o a) where
    random = first NetOL . random
    randomR (NetOL rmn, NetOL rmx) = first NetOL . randomR (rmn, rmx)

instance (KnownNat i, KnownNat o, KnownNat j, Random a, Random (Network j hs o a)) => Random (Network i (j ': hs) o a) where
    random g = let (l, g') = random g
               in  first (l `NetIL`) (random g')
    randomR (NetIL lmn nmn, NetIL lmx nmx) g =
        let (l , g') = randomR (lmn, lmx) g
        in  first (l `NetIL`) (randomR (nmn, nmx) g')

instance (KnownNat i, KnownNat o, B.Binary a) => B.Binary (Network i '[] o a) where
    put (NetOL l) = B.put l
    get = NetOL <$> B.get

instance (KnownNat i, KnownNat o, KnownNat j, B.Binary a, B.Binary (Network j hs o a)) => B.Binary (Network i (j ': hs) o a) where
    put (NetIL l n') = B.put l *> B.put n'
    get = NetIL <$> B.get <*> B.get

instance NFData a => NFData (Network i hs o a) where
    rnf (NetOL (force -> !_)) = ()
    rnf (NetIL (force -> !_) (force -> !_)) = ()

deriving instance Show a => Show (Network i hs o a)
deriving instance Foldable (Network i hs o)
deriving instance Traversable (Network i hs o)

