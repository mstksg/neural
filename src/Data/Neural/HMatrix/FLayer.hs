{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Neural.HMatrix.FLayer where

import           Control.DeepSeq
import           Control.Monad.Primitive
import           Control.Monad.Random         as R
import           Data.Foldable
import           Data.MonoTraversable
import           Data.Neural.HMatrix.Utility
import           Data.Proxy
import           Data.Reflection
import           GHC.Generics                 (Generic)
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import           System.Random.MWC hiding     (create)
import qualified Data.Binary                  as B
import qualified Data.Neural.Types            as N
import qualified Data.Vector                  as V
import qualified Data.Vector.Generic          as VG
import qualified Linear.V                     as L
import qualified Numeric.LinearAlgebra        as H

data FLayer :: Nat -> Nat -> * where
    FLayer :: { fLayerBiases  :: !(R o)
              , fLayerWeights :: !(L o i)
              } -> FLayer i o
  deriving (Show, Generic)

data SomeFLayer :: * where
    SomeFLayer :: (KnownNat i, KnownNat o) => FLayer i o -> SomeFLayer

type instance Element (FLayer i o) = Double

instance (KnownNat i, KnownNat o) => MonoFunctor (FLayer i o) where
    omap f (FLayer b w) = FLayer (dvmap f b) (dmmap f w)

deriving instance Show SomeFLayer

-- instance MonoZpureNetip (FLayer i o) where
--     ozipWith f (FLayer b1 w1) (FLayer b2 w2) = FLayer ()

konstFLayer :: (KnownNat i, KnownNat o)
            => Double
            -> FLayer i o
konstFLayer = FLayer <$> konst <*> konst

instance (KnownNat i, KnownNat o) => Num (FLayer i o) where
    FLayer b1 w1 + FLayer b2 w2 = FLayer (b1 + b2) (w1 + w2)
    FLayer b1 w1 * FLayer b2 w2 = FLayer (b1 * b2) (w1 * w2)
    FLayer b1 w1 - FLayer b2 w2 = FLayer (b1 - b2) (w1 - w2)
    abs (FLayer b w) = FLayer (abs b) (abs w)
    negate (FLayer b w) = FLayer (negate b) (negate w)
    signum (FLayer b w) = FLayer (signum b) (signum w)
    fromInteger = FLayer <$> fromInteger <*> fromInteger

instance (KnownNat i, KnownNat o) => Fractional (FLayer i o) where
    FLayer b1 w1 / FLayer b2 w2 = FLayer (b1 / b2) (w1 / w2)
    recip        = omap recip
    fromRational = konstFLayer . fromRational

instance (KnownNat i, KnownNat o) => Random (FLayer i o) where
    random = runRand $
        FLayer <$> randomVec (-1, 1)
               <*> randomMat (-1, 1)
    randomR  = error "FLayer i o (randomR): Unimplemented"

instance NFData (FLayer i o)

instance (KnownNat i, KnownNat o) => B.Binary (FLayer i o) where

instance B.Binary SomeFLayer where
    put sl = case sl of
               SomeFLayer (l :: FLayer i o) -> do
                 B.put $ natVal (Proxy :: Proxy i)
                 B.put $ natVal (Proxy :: Proxy o)
                 B.put l
    get = do
      i <- B.get
      o <- B.get
      reifyNat i $ \(Proxy :: Proxy i) ->
        reifyNat o $ \(Proxy :: Proxy o) ->
          SomeFLayer <$> (B.get :: B.Get (FLayer i o))

runFLayer :: (KnownNat i, KnownNat o) => FLayer i o -> R i -> R o
runFLayer (FLayer b w) v = b + w #> v
{-# INLINE runFLayer #-}

fLayerFromHMat :: (KnownNat i, KnownNat o) => FLayer i o ->  N.FLayer i o Double
fLayerFromHMat (FLayer b w) = N.FLayer . L.V . V.fromList $ zipWith N.Node bl wl
  where
    bl = H.toList (extract b)
    wl = map (L.V . VG.convert . extract) $ toRows w

fLayerFromV :: (KnownNat i, KnownNat o) => N.FLayer i o Double -> FLayer i o
fLayerFromV (N.FLayer n) = FLayer b w
  where
    Just b = create . VG.convert . L.toVector $ N.nodeBias <$> n
    Just w = create . H.fromRows . toList $ VG.convert . L.toVector . N.nodeWeights <$> n

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

