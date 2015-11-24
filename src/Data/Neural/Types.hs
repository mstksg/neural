{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Neural.Types where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Monad.Trans.State
import Data.Foldable
import Data.Monoid
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Linear
import Linear.V
import System.Random
import GHC.Exts (Constraint)
import qualified Data.Binary     as B
import qualified Data.Vector     as V

-- | Types

data Node :: Nat -> * -> * where
    Node :: { nodeBias :: !a, nodeWeights :: !(V i a) } -> Node i a
  deriving (Show, Generic)

newtype FLayer :: Nat -> Nat -> * -> * where
    FLayer :: { layerNodes :: V o (Node i a) } -> FLayer i o a
  deriving (Show, Foldable, Traversable, Functor, Generic)

data Network :: (Nat -> Nat -> * -> *)
             -> Nat -> [Nat] -> Nat -> *
             -> * where
    NetOL :: !(l i o a) -> Network l i '[] o a
    NetIL :: KnownNat j => !(l i j a) -> !(Network l j hs o a) -> Network l i (j ': hs) o a

infixr 5 `NetIL`

-- | Classes

class Layer (l :: Nat -> Nat -> * -> *) where
    type LayerOut l (i :: Nat) (j :: Nat) (a :: *) :: *
    type LayerRun l :: * -> Constraint
    layerMap :: Layer l => (a -> b) -> l i o a -> l i o b
    layerPure :: Layer l => a -> l i o a
    layerAp :: Layer l => l i o (a -> b) -> l i o a -> l i o b
    runLayer :: (Layer l, LayerRun l a, KnownNat i)
             => l i o a -> V i a -> LayerOut l i o a
    layerRandom :: (Random a, RandomGen g) => g -> l i o a
    layerRandomR :: (Random a, RandomGen g) => (l i o a, l i o a) -> g -> l i o a

-- | Instances
--
-- | * V i

instance (KnownNat i, Random a) => Random (V i a) where
    random g = first V . flip runState g
             $ V.replicateM (reflectDim (Proxy :: Proxy i)) (state random)
    randomR (V rmn, V rmx) g = first V . flip runState g
                             $ V.zipWithM (\x y -> state (randomR (x, y))) rmn rmx

-- | * Node

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

instance Foldable (Node i) where
    fold (Node b w) = b <> fold w
    {-# INLINE fold #-}
    foldMap f (Node b w) = f b <> foldMap f w
    {-# INLINE foldMap #-}
    foldr f z (Node b w) = b `f` foldr f z w
    {-# INLINE foldr #-}
    foldl f z (Node b w) = foldl f (f z b) w
    {-# INLINE foldl #-}
    foldl' f z (Node b w) = let z' = f z b in z' `seq` foldl f z' w
    {-# INLINE foldl' #-}
    -- foldr1 f (Node b w) = ???
    foldl1 f (Node b w) = foldl f b w
    {-# INLINE foldl1 #-}
    toList (Node b w) = b : toList w
    {-# INLINE toList #-}
    null _ = False
    {-# INLINE null #-}
    length (Node _ w) = 1 + length w
    {-# INLINE length #-}
    elem x (Node b w) = (x == b) || elem x w
    {-# INLINE elem #-}
    maximum (Node b w) = b `max` maximum w
    {-# INLINE maximum #-}
    minimum (Node b w) = b `min` minimum w
    {-# INLINE minimum #-}
    sum (Node b w) = b + sum w
    {-# INLINE sum #-}
    product (Node b w) = b * product w
    {-# INLINE product #-}

instance Traversable (Node i) where
    traverse f (Node b w) = Node <$> f b <*> traverse f w
    {-# INLINE traverse #-}
    sequenceA (Node b w) = Node <$> b <*> sequenceA w
    {-# INLINE sequenceA #-}
    mapM = traverse
    {-# INLINE mapM #-}
    sequence = sequenceA
    {-# INLINE sequence #-}

instance (KnownNat i, Random a) => Random (Node i a) where
    random g =
        let (b, g') = random g
        in  first (Node b) (random g')
    randomR (Node bmn wmn, Node bmx wmx) g =
        let (b, g') = randomR (bmn, bmx) g
        in  first (Node b) (randomR (wmn, wmx) g')

instance (KnownNat i, B.Binary a) => B.Binary (Node i a)

instance NFData a => NFData (Node i a)

-- | * FLayer

instance NFData a => NFData (FLayer i o a)
instance (KnownNat i, KnownNat o, B.Binary a) => B.Binary (FLayer i o a)

instance (KnownNat i, KnownNat o) => Applicative (FLayer i o) where
    pure = FLayer . V . V.replicate (reflectDim (Proxy :: Proxy o)) . pure
    {-# INLINE pure #-}
    FLayer f <*> FLayer x = FLayer (liftA2 (<*>) f x)
    {-# INLINE (<*>) #-}


deriving instance (KnownNat i, KnownNat o, Random a) => Random (FLayer i o a)

instance Layer FLayer where
    type LayerOut FLayer i o a = V o a
    type LayerRun FLayer = Num
    layerMap = fmap
    {-# INLINE layerMap #-}
    runLayer (FLayer l) v = l !* Node 1 v
    {-# INLINE runLayer #-}

-- | * Network

instance Layer l => Functor (Network l i hs o) where
    fmap f n = case n of
                 NetOL l    -> NetOL (layerMap f l)
                 NetIL l n' -> layerMap f l `NetIL` fmap f n'
    {-# INLINE fmap #-}

instance (Layer l, KnownNat i, KnownNat o) => Applicative (Network l i '[] o) where
    pure = NetOL . layerPure
    {-# INLINE pure #-}
    NetOL f <*> NetOL x = NetOL (layerAp f x)
    {-# INLINE (<*>) #-}

instance (Layer l, KnownNat i, KnownNat o, KnownNat j, Applicative (Network l j hs o)) => Applicative (Network l i (j ': hs) o) where
    pure x = layerPure x `NetIL` pure x
    {-# INLINE pure #-}
    NetIL fi fr <*> NetIL xi xr = NetIL (layerAp fi xi) (fr <*> xr)
    {-# INLINE (<*>) #-}

instance (Layer l, KnownNat i, KnownNat o, Random a) => Random (Network l i '[] o a) where
    random = first NetOL . random
    randomR (NetOL rmn, NetOL rmx) = first NetOL . randomR (rmn, rmx)

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
