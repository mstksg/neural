{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Data.Neural.HMatrix.FeedForward where

import Data.Neural.HMatrix.Types
import GHC.TypeLits
import GHC.TypeLits.List

data Network :: Nat -> [Nat] -> Nat -> * where
    NetOL :: !(FLayer i o) -> Network i '[] o
    NetIL :: (KnownNat j, KnownNats hs)
          => !(FLayer i j) -> !(Network j hs o) -> Network i (j ': hs) o

