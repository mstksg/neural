{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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
import Data.Constraint
import Data.Foldable
import Data.Monoid
import Data.Proxy
import Data.Reflection
import Data.Type.Product
import GHC.Generics
import GHC.TypeLits
import GHC.TypeLits.List
import Linear
import Linear.V
import System.Random
import Type.Class.Known
import qualified Data.Binary     as B
import qualified Data.Vector     as V

-- | Types

data Node :: Nat -> * -> * where
    Node :: { nodeBias :: !a, nodeWeights :: !(V i a) } -> Node i a
  deriving (Show, Generic)

newtype FLayer :: Nat -> Nat -> * -> * where
    FLayer :: { layerNodes :: V o (Node i a) } -> FLayer i o a
  deriving (Show, Foldable, Traversable, Functor, Generic)

data NeuralActs :: * -> * where
    NA :: { naInner :: a -> a, naOuter :: a -> a } -> NeuralActs a

data NetworkG :: (Nat -> Nat -> * -> *) -> Nat -> [Nat] -> Nat -> * -> * where
    NetOL :: !(FLayer i o a) -> NetworkG l i '[] o a
    NetIL :: KnownNat j => !(l i j a) -> !(NetworkG l j hs o a) -> NetworkG l i (j ': hs) o a

infixr 5 `NetIL`


data SomeFLayer :: * -> * where
    SomeFLayer :: (KnownNat i, KnownNat o) => FLayer i o a -> SomeFLayer a

data NetStruct :: * where
    NetStruct :: (KnownNat i, KnownNat o, KnownNats hs)
              => Proxy i
              -> Prod Proxy hs
              -> Proxy o
              -> NetStruct

type KnownNet i hs o = (KnownNat i, KnownNats hs, Known (Prod Proxy) hs, KnownNat o)

class OnNats (f :: (* -> *) -> Constraint) (l :: Nat -> Nat -> * -> *) where
    onNats :: (KnownNat i, KnownNat o) => Dict (f (l i o))

class OnNats0 (f :: * -> Constraint) (l :: Nat -> Nat -> * -> *) (a :: *) where
    onNats0 :: (KnownNat i, KnownNat o) => Dict (f (l i o a))


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

instance OnNats Functor FLayer where
    onNats = Dict


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


instance (OnNats Functor l, KnownNet i hs o) => Functor (NetworkG l i hs o) where
    fmap f n = case n of
                 NetOL l                 ->
                   NetOL (fmap f l)
                 NetIL (l :: l i j a) n' ->
                   case onNats :: Dict (Functor (l i j)) of
                     Dict -> fmap f l `NetIL` fmap f n'

instance (OnNats Functor l, OnNats Applicative l, KnownNet i hs o) => Applicative (NetworkG l i hs o) where
    pure x = case known :: Prod Proxy hs of
               Ø        -> NetOL (pure x)
               (_ :: Proxy j) :< _
                 -> case onNats :: Dict (Applicative (l i j)) of
                      Dict -> pure x `NetIL` pure x
    {-# INLINE pure #-}
    NetOL f <*> NetOL x = NetOL (f <*> x)
    NetIL (fi :: l i j (a -> b)) fr <*> NetIL xi xr =
      case onNats :: Dict (Applicative (l i j)) of
        Dict -> NetIL (fi <*> xi) (fr <*> xr)
    _           <*> _           = error "this should never happen"
    {-# INLINE (<*>) #-}

instance Applicative (NetworkG l i hs o) => Additive (NetworkG l i hs o) where
    zero = pure 0
    {-# INLINE zero #-}
    (^+^) = liftA2 (+)
    {-# INLINE (^+^) #-}
    (^-^) = liftA2 (-)
    {-# INLINE (^-^) #-}
    liftU2 = liftA2
    {-# INLINE liftU2 #-}
    liftI2 = liftA2
    {-# INLINE liftI2 #-}

instance (OnNats Functor l, OnNats Applicative l, OnNats Foldable l, KnownNet i hs o) => Metric (NetworkG l i hs o)

instance (OnNats Foldable l, KnownNet i hs o) => Foldable (NetworkG l i hs o) where
    foldMap f n = case n of
                    NetOL l    -> foldMap f l
                    NetIL (l :: l i j a) n' ->
                      case onNats :: Dict (Foldable (l i j)) of
                        Dict -> foldMap f l <> foldMap f n'

instance (OnNats Functor l, OnNats Foldable l, OnNats Traversable l, KnownNet i hs o) => Traversable (NetworkG l i hs o) where
    traverse f n = case n of
                     NetOL l -> NetOL <$> traverse f l
                     NetIL (l :: l i j a) n' ->
                       case onNats :: Dict (Traversable (l i j)) of
                         Dict -> NetIL <$> traverse f l <*> traverse f n'

instance (OnNats0 Random l a, OnNats Functor l, OnNats Applicative l, OnNats Foldable l, OnNats Traversable l, Random a, KnownNet i hs o) => Random (NetworkG l i hs o a) where
    random = runState $
      case known :: Prod Proxy hs of
        Ø                   ->
          NetOL <$> state random
        (_ :: Proxy j) :< _ ->
          case onNats0 :: Dict (Random (l i j a)) of
             Dict -> NetIL <$> state random <*> state random
    randomR rng = runState $
      case rng of
        (NetOL rmn, NetOL rmx)         -> NetOL <$> state (randomR (rmn, rmx))
        (NetIL (lmn :: l i j a) nmn, NetIL lmx nmx) ->
          case onNats0 :: Dict (Random (l i j a)) of
            Dict -> NetIL <$> state (randomR (lmn, lmx))
                          <*> state (randomR (nmn, nmx))
        (_, _) -> error "impossible!"

instance (B.Binary a, OnNats0 B.Binary l a, KnownNet i hs o) => B.Binary (NetworkG l i hs o a) where
    put n = case n of
              NetOL l -> B.put l
              NetIL (l :: l i j a) n' ->
                case onNats0 :: Dict (B.Binary (l i j a)) of
                  Dict -> B.put l *> B.put n'
    get = case known :: Prod Proxy hs of
            Ø -> NetOL <$> B.get
            (_ :: Proxy j) :< _ ->
              case onNats0 :: Dict (B.Binary (l i j a)) of
                Dict -> NetIL <$> B.get <*> B.get

instance (OnNats0 NFData l a, NFData a, KnownNet i hs o) => NFData (NetworkG l i hs o a) where
    rnf n = case n of
              NetOL l -> l `deepseq` ()
              NetIL (l :: l i j a) n' ->
                case onNats0 :: Dict (NFData (l i j a)) of
                  Dict -> l `deepseq` n' `deepseq` ()

-- deriving instance Show a => Show (NetworkG l i hs o a)

