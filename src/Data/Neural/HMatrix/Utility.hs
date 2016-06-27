{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Neural.HMatrix.Utility
  ( randomVec, randomMat
  , randomVecMWC, randomMatMWC
  )
  where

-- import           Control.Monad
-- import qualified Numeric.LinearAlgebra     as H
import           Control.Lens                 ((<&>))
import           Control.Monad.Primitive
import           Control.Monad.Random         as R
import           Data.Proxy
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import qualified System.Random.MWC            as MWC

randomVec :: forall n m. (MonadRandom m, KnownNat n)
          => (Double, Double) -> m (R n)
randomVec = fmap unrow . randomMat

randomMat :: forall f n m. (MonadRandom f, KnownNat n, KnownNat m)
          => (Double, Double) -> f (L n m)
randomMat (mn, mx) = getRandom
                 <&> \i -> uniformSample i (konst mn) (konst mx)

randomVecMWC :: forall n m. (PrimMonad m, KnownNat n)
             => (Double, Double) -> MWC.Gen (PrimState m) -> m (R n)
randomVecMWC r g = unrow <$> randomMatMWC r g

randomMatMWC :: forall f n m. (PrimMonad f, KnownNat n, KnownNat m)
             => (Double, Double) -> MWC.Gen (PrimState f) -> f (L n m)
randomMatMWC (mn, mx) g = MWC.uniform g
                 <&> \i -> uniformSample i (konst mn) (konst mx)
