{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Neural.HMatrix.Utility where

import Control.Lens                    ((<&>))
import Control.Monad
import Control.Monad.Random            as R
import Data.Proxy
import GHC.TypeLits
import Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra as H

randomVec :: forall n m. (MonadRandom m, KnownNat n)
          => (Double, Double) -> m (R n)
randomVec = fmap unrow . randomMat

randomMat :: forall f n m. (MonadRandom f, KnownNat n, KnownNat m)
          => (Double, Double) -> f (L n m)
randomMat (mn, mx) = getRandom
                 <&> \i -> uniformSample i (konst mn) (konst mx)
  where
    s = natVal (Proxy :: Proxy n) * natVal (Proxy :: Proxy m)

