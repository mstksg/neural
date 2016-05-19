{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Neural.HMatrix.Recurrent.Generate
  ( randomFLayer, randomRLayer, randomNet
  , randomFLayerMWC, randomRLayerMWC, randomNetMWC
  )
  where

import Control.Monad.Primitive
import Control.Monad.Random
import Data.Neural.HMatrix.Recurrent
import Data.Neural.HMatrix.Utility
import Data.Neural.Types             (KnownNet)
import GHC.TypeLits
import GHC.TypeLits.List
import System.Random.MWC

randomFLayer
    :: forall m i o. (MonadRandom m, KnownNat i, KnownNat o)
    => (Double, Double)
    -> m (FLayer i o)
randomFLayer r = FLayer <$> randomVec r
                        <*> randomMat r

randomFLayerMWC
    :: forall m i o. (PrimMonad m, KnownNat i, KnownNat o)
    => (Double, Double)
    -> Gen (PrimState m)
    -> m (FLayer i o)
randomFLayerMWC r g = FLayer <$> randomVecMWC r g
                             <*> randomMatMWC r g


randomRLayer
    :: forall m i o. (MonadRandom m, KnownNat i, KnownNat o)
    => (Double, Double)
    -> m (RLayer i o)
randomRLayer r = RLayer <$> randomVec r
                        <*> randomMat r
                        <*> randomMat r
                        <*> randomVec r

randomRLayerMWC
    :: forall m i o. (PrimMonad m, KnownNat i, KnownNat o)
    => (Double, Double)
    -> Gen (PrimState m)
    -> m (RLayer i o)
randomRLayerMWC r g = RLayer <$> randomVecMWC r g
                             <*> randomMatMWC r g
                             <*> randomMatMWC r g
                             <*> randomVecMWC r g


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

randomNetMWC
    :: forall m i hs o. (PrimMonad m, KnownNet i hs o)
    => (Double, Double)
    -> Gen (PrimState m)
    -> m (Network i hs o)
randomNetMWC r g = go natsList
  where
    go :: forall j js. KnownNat j
       => NatList js
       -> m (Network j js o)
    go nl = case nl of
              ØNL       -> NetOL <$> randomFLayerMWC r g
              _ :<# nl' -> NetIL <$> randomRLayerMWC r g <*> go nl'

