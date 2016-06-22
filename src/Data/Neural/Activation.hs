{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Neural.Activation where

import GHC.Generics
import qualified Data.Binary as B

data NeuralActs :: * -> * where
    NA :: { naInternal :: a -> a, naOuter :: a -> a } -> NeuralActs a
  deriving (Generic)

-- rewrite using fix?
data Activation :: * where
    IdAct     :: Activation
    Logistic  :: Activation
    ReLU      :: Activation
    LReLU     :: !Rational -> Activation
    SoftPlus  :: Activation
    ScaledAct :: !Rational -> !Activation -> Activation
    ExpandAct :: !Rational -> !Activation -> Activation
  deriving (Show, Eq, Read, Generic)

data NASpec :: * where
    NASpec :: !Activation -> !Activation -> NASpec
  deriving (Show, Eq, Read, Generic)

instance B.Binary Activation
instance B.Binary NASpec

-- normalize to get rid of Scaled (Expand (Scaled ..))
reduceAct :: Activation -> Activation
reduceAct (ScaledAct s a) = case a of
                              ScaledAct s' a' ->
                                reduceAct $ ScaledAct (s * s') a'
                              _              ->
                                ScaledAct s $ reduceAct a
reduceAct (ExpandAct s a) = case a of
                              ExpandAct s' a' ->
                                reduceAct $ ExpandAct (s * s') a'
                              _              ->
                                ExpandAct s $ reduceAct a
reduceAct a               = a


toFunc :: (Floating a, Ord a) => Activation -> a -> a
toFunc IdAct           = id
toFunc Logistic        = logistic
toFunc ReLU            = relu
toFunc (LReLU k)       = lRelu (fromRational k)
toFunc SoftPlus        = softplus
toFunc (ScaledAct s a) = (* fromRational s) . toFunc a
toFunc (ExpandAct s a) = toFunc a . (/ fromRational s)

toFunc' :: (Floating a, Ord a) => Activation -> a -> a
toFunc' = toFunc . reduceAct

toNeuralActs :: (Floating a, Ord a) => NASpec -> NeuralActs a
toNeuralActs (NASpec f g) = NA (toFunc f) (toFunc g)

toNeuralActs' :: (Floating a, Ord a) => NASpec -> NeuralActs a
toNeuralActs' (NASpec f g) = NA (toFunc' f) (toFunc' g)


logistic :: Floating a => a -> a
logistic x = 1 / (exp (-x) + 1)

relu :: (Num a, Ord a) => a -> a
relu = max 0

lRelu :: (Num a, Ord a) => a -> a -> a
lRelu a x = max (a * x) x

softplus :: Floating a => a -> a
softplus x = log (1 + exp x)

naLogId :: Floating a => NeuralActs a
naLogId = naWithId logistic
{-# INLINE naLogId #-}

naLogLog :: Floating a => NeuralActs a
naLogLog = naUniform logistic
{-# INLINE naLogLog #-}

naRLId :: (Num a, Ord a) => NeuralActs a
naRLId = naWithId relu
{-# INLINE naRLId #-}

naRLRL :: (Num a, Ord a) => NeuralActs a
naRLRL = naUniform relu
{-# INLINE naRLRL #-}

naRLLog :: (Floating a, Ord a) => NeuralActs a
naRLLog = NA relu logistic
{-# INLINE naRLLog #-}

naSPId :: Floating a => NeuralActs a
naSPId = naWithId softplus
{-# INLINE naSPId #-}

naSPSP :: Floating a => NeuralActs a
naSPSP = naUniform softplus
{-# INLINE naSPSP #-}

naUniform :: (a -> a) -> NeuralActs a
naUniform f = NA f f
{-# INLINE naUniform #-}

naWithId :: (a -> a) -> NeuralActs a
naWithId f = NA f id
{-# INLINE naWithId #-}
