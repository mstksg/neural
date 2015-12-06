{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Neural.Types where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Monad.Trans.State
import Data.Foldable
import GHC.TypeLits.List
import Data.Monoid
import Data.Proxy
import Data.Reflection
import Data.Type.Product
import GHC.Generics
import GHC.TypeLits
import Linear
import Linear.V
import System.Random
import qualified Data.Binary      as B
import qualified Data.Vector      as V

-- | Types

data Node :: Nat -> * -> * where
    Node :: { nodeBias :: !a, nodeWeights :: !(V i a) } -> Node i a
  deriving (Show, Generic)

newtype FLayer :: Nat -> Nat -> * -> * where
    FLayer :: { layerNodes :: V o (Node i a) } -> FLayer i o a
  deriving (Show, Foldable, Traversable, Functor, Generic)

data NeuralActs :: * -> * where
    NA :: { naInner :: a -> a, naOuter :: a -> a } -> NeuralActs a

data SomeFLayer :: * -> * where
    SomeFLayer :: (KnownNat i, KnownNat o) => FLayer i o a -> SomeFLayer a

data NetStruct :: * where
    NetStruct :: KnownNet i hs o
              => Proxy i
              -> Prod Proxy hs
              -> Proxy o
              -> NetStruct

type KnownNet i hs o = (KnownNat i, KnownNats hs, KnownNat o)

-- deriving instance Show NetStruct


-- | Class

-- class Layer (l :: Nat -> Nat -> * -> *) where
--     type LayerOut l (o :: Nat) a :: *
--     type RunConstraint l :: * -> Constraint
--     runLayer :: (KnownNat i, RunConstraint l a) => l i o a -> V i a -> LayerOut l o a

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
    Node b1 w1 ^+^ Node b2 w2 = Node (b1 + b2) (w1 ^+^ w2)
    {-# INLINE (^+^) #-}
    Node b1 w1 ^-^ Node b2 w2 = Node (b1 - b2) (w1 ^-^ w2)
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
    pure = FLayer . pure . pure
    {-# INLINE pure #-}
    FLayer f <*> FLayer x = FLayer (liftA2 (<*>) f x)
    {-# INLINE (<*>) #-}

deriving instance (KnownNat i, KnownNat o, Random a) => Random (FLayer i o a)

-- | * SomeFLayer

deriving instance Show a => Show (SomeFLayer a)
deriving instance Functor SomeFLayer
deriving instance Foldable SomeFLayer
deriving instance Traversable SomeFLayer

instance B.Binary a => B.Binary (SomeFLayer a) where
    put sl = case sl of
               SomeFLayer (l :: FLayer i o a) -> do
                 B.put $ natVal (Proxy :: Proxy i)
                 B.put $ natVal (Proxy :: Proxy o)
                 B.put l
    get = do
      i <- B.get
      o <- B.get
      reifyNat i $ \(Proxy :: Proxy i) ->
        reifyNat o $ \(Proxy :: Proxy o) ->
          SomeFLayer <$> (B.get :: B.Get (FLayer i o a))




-- | * Network

-- fmapNetwork :: forall a b l i hs o.
--                (forall a' b' i' o'. (a' -> b') -> l i' o' a' -> l i' o' b')
--             -> (a -> b)
--             -> Network l i hs o a
--             -> Network l i hs o b
-- fmapNetwork f g = go
--   where
--     go :: forall i' hs' o'. Network l i' hs' o' a -> Network l i' hs' o' b
--     go n = case n of
--              NetOL l    -> NetOL (f g l)
--              NetIL l n' -> NetIL (f g l) (go n')

-- pureNetworkO :: forall a l i o.
--                 (forall a'. a' -> l i o a')
--              -> a -> Network l i '[] o a
-- pureNetworkO p = NetOL . p

-- apNetworkO :: forall a b l i o.
--               (forall a' b'. l i o (a' -> b') -> l i o a' -> l i o b')
--            -> Network l i '[] o (a -> b)
--            -> Network l i '[] o a
--            -> Network l i '[] o b
-- apNetworkO p (NetOL f) (NetOL x) = NetOL (p f x)

-- pureNetworkI :: forall a l i h hs o. KnownNat h
--              => (forall a'. a' -> l i h a')
--              -> (forall a'. a' -> Network l h hs o a')
--              -> a
--              -> Network l i (h ': hs) o a
-- pureNetworkI p pN x = p x `NetIL` pN x

-- apNetworkI :: forall a b l i h hs o.
--               (forall a' b'. l i h (a' -> b') -> l i h a' -> l i h b')
--            -> (forall a' b'. Network l h hs o (a' -> b') -> Network l h hs o a' -> Network l h hs o b')
--            -> Network l i (h ': hs) o (a -> b)
--            -> Network l i (h ': hs) o a
--            -> Network l i (h ': hs) o b
-- apNetworkI a aN (NetIL l n) (NetIL l' n') = NetIL (a l l') (aN n n')

-- instance (KnownNat i, KnownNat o, Random a) => Random (Network i '[] o a) where
--     random = first OLayer . random
--     randomR (OLayer rmn, OLayer rmx) = first OLayer . randomR (rmn, rmx)


