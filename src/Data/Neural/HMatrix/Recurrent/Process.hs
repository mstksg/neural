{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveFunctor       #-}

module Data.Neural.HMatrix.Recurrent.Process where

import           Data.Bifunctor
import           Data.List                    (foldl')
import           Data.Proxy
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import qualified Data.Vector                  as V
import qualified Linear.V                     as L

processSeries
    :: forall a b n. KnownNat n
    => V.Vector (a, b)
    -> V.Vector (L.V n a, b)
processSeries ios = fmap (bimap (L.V . V.take n) (V.! n) . V.unzip)
                  . foldl' (V.zipWith V.snoc) (V.replicate k V.empty)
                  . V.iterateN (n+1) (V.drop 1)
                  $ ios
  where
    n = fromInteger $ natVal (Proxy :: Proxy n)
    m = V.length ios
    k = m - n + 1

