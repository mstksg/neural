{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import qualified Data.Binary     as B
import qualified Data.Vector     as V

data Node :: Nat -> * -> * where
    Node :: { nodeBias :: !a, nodeWeights :: !(V i a) } -> Node i a
  deriving (Show, Generic)

newtype Layer :: Nat -> Nat -> * -> * where
    Layer :: { layerNodes :: V o (Node i a) } -> Layer i o a
  deriving (Show, Foldable, Traversable, Functor, Generic)

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

instance (KnownNat i, KnownNat o) => Applicative (Layer i o) where
    pure = Layer . V . V.replicate (reflectDim (Proxy :: Proxy o)) . pure
    {-# INLINE pure #-}
    Layer f <*> Layer x = Layer (liftA2 (<*>) f x)
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

instance (KnownNat i, B.Binary a) => B.Binary (Node i a)
instance (KnownNat i, KnownNat o, B.Binary a) => B.Binary (Layer i o a)

instance NFData a => NFData (Node i a)
instance NFData a => NFData (Layer i o a)

deriving instance (KnownNat i, KnownNat o, Random a) => Random (Layer i o a)

