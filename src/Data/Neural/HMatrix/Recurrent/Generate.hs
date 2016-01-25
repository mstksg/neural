{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}

module Data.Neural.HMatrix.Recurrent.Generate where

import Control.Monad.Random
import Data.Neural.HMatrix.Recurrent
import Data.Neural.HMatrix.Utility
import Data.Neural.Types             (KnownNet)
import GHC.TypeLits
import GHC.TypeLits.List

randomFLayer
    :: forall m i o. (MonadRandom m, KnownNat i, KnownNat o)
    => (Double, Double)
    -> m (FLayer i o)
randomFLayer r = FLayer <$> randomVec r
                        <*> randomMat r

randomRLayer
    :: forall m i o. (MonadRandom m, KnownNat i, KnownNat o)
    => (Double, Double)
    -> m (RLayer i o)
randomRLayer r = RLayer <$> randomVec r
                        <*> randomMat r
                        <*> randomMat r
                        <*> randomVec r

randomNet
    :: forall m i hs o. (MonadRandom m, KnownNet i hs o)
    => (Double, Double)
    -> m (Network i hs o)
randomNet r = go natsList
  where
    go :: forall j js. KnownNat j
       => NatList js
       -> m (Network j js o)
    go nl = case nl of
              ØNL       -> NetOL <$> randomFLayer r
              _ :<# nl' -> NetIL <$> randomRLayer r <*> go nl'

