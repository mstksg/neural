{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Neural.HMatrix.Recurrent where

import Control.DeepSeq
import Control.Monad.Random         as R
import Control.Monad.State
import Data.Neural.Types            (KnownNet, NeuralActs(..))
import Data.Proxy
import Data.Reflection
import GHC.Generics                 (Generic)
import GHC.TypeLits
import GHC.TypeLits.List
import Numeric.LinearAlgebra.Static
import qualified Data.Binary        as B

data FLayer :: Nat -> Nat -> * where
    FLayer :: { fLayerBiases  :: !(R o)
              , fLayerWeights :: !(L o i)
              } -> FLayer i o
  deriving (Show, Generic)


data RLayer :: Nat -> Nat -> * where
    RLayer :: { rLayerBiases   :: !(R o)
              , rLayerIWeights :: !(L o i)
              , rLayerSWeights :: !(L o o)
              , rLayerState    :: !(R o)
              } -> RLayer i o
  deriving (Show, Generic)

data Network :: Nat -> [Nat] -> Nat -> * where
    NetOL :: !(FLayer i o) -> Network i '[] o
    NetIL :: (KnownNat j, KnownNats hs)
          => !(RLayer i j) -> !(Network j hs o) -> Network i (j ': hs) o

infixr 5 `NetIL`

data NetActs :: Nat -> [Nat] -> Nat -> * where
    NetAOL :: !(R o) -> NetActs i hs o
    NetAIL :: (KnownNat j, KnownNats hs) => !(R j) -> !(NetActs j hs o) -> NetActs i (j ': js) o

infixr 5 `NetAIL`

data SomeNet :: * where
    SomeNet :: KnownNet i hs o => Network i hs o -> SomeNet

data OpaqueNet :: Nat -> Nat -> * where
    OpaqueNet :: KnownNats hs => Network i hs o -> OpaqueNet i o

data SomeFLayer :: * where
    SomeFLayer :: (KnownNat i, KnownNat o) => FLayer i o -> SomeFLayer

deriving instance KnownNet i hs o => Show (Network i hs o)
deriving instance KnownNet i hs o => Show (NetActs i hs o)
deriving instance Show SomeNet
deriving instance (KnownNat i, KnownNat o) => Show (OpaqueNet i o)
deriving instance Show SomeFLayer

randomVec :: (RandomGen g, KnownNat n)
          => (Double, Double) -> g -> (R n, g)
randomVec r g = let (g', g'') = R.split g
                in  (vector (randomRs r g'), g'')

randomMat :: (RandomGen g, KnownNat n, KnownNat m)
          => (Double, Double) -> g -> (L n m, g)
randomMat r g = let (g', g'') = R.split g
                in  (matrix (randomRs r g'), g'')

instance (KnownNat i, KnownNat o) => Random (FLayer i o) where
    random = runRand $
        FLayer <$> liftRand (randomVec (-1, 1))
               <*> liftRand (randomMat (-1, 1))
    randomR  = error "FLayer i o (randomR): Unimplemented"

instance (KnownNat i, KnownNat o) => Random (RLayer i o) where
    random = runRand $
        RLayer <$> liftRand (randomVec (-1, 1))
               <*> liftRand (randomMat (-1, 1))
               <*> liftRand (randomMat (-1, 1))
               <*> liftRand (randomVec (-1, 1))
    randomR  = error "RLayer i o (randomR): Unimplemented"

instance NFData (FLayer i o)
instance NFData (RLayer i o)

instance NFData (Network i hs o) where
    rnf (NetOL (force -> !_)) = ()
    rnf (NetIL (force -> !_) (force -> !_)) = ()

instance NFData (NetActs i hs o) where
    rnf (NetAOL (force -> !_)) = ()
    rnf (NetAIL (force -> !_) (force -> !_)) = ()

instance (KnownNat i, KnownNat o) => B.Binary (FLayer i o) where
instance (KnownNat i, KnownNat o) => B.Binary (RLayer i o) where

instance (KnownNat n, KnownNat m) => B.Binary (L n m) where
    put l = B.put (unwrap l)
    get   = do
      m <- B.get
      case create m of
        Just l  -> return l
        Nothing -> fail "L: Improper stored matrix size"

instance (KnownNat n) => B.Binary (R n) where
    put l = B.put (unwrap l)
    get   = do
      v <- B.get
      case create v of
        Just l  -> return l
        Nothing -> fail "R: Improper stored vector size"

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

instance KnownNet i hs o => B.Binary (Network i hs o) where
    put (NetOL l)    = B.put l
    put (NetIL l n') = B.put l *> B.put n'
    get = case natsList :: NatList hs of
            ØNL     -> NetOL <$> B.get
            _ :<# _ -> NetIL <$> B.get <*> B.get

instance B.Binary SomeNet where
    put sn = case sn of
               SomeNet (n :: Network i hs o) -> do
                 B.put $ natVal (Proxy :: Proxy i)
                 B.put $ natVal (Proxy :: Proxy o)
                 B.put $ OpaqueNet n
    get = do
      i <- B.get
      o <- B.get
      reifyNat i $ \(Proxy :: Proxy i) ->
        reifyNat o $ \(Proxy :: Proxy o) -> do
          oqn <- B.get :: B.Get (OpaqueNet i o)
          return $ case oqn of
                     OpaqueNet n -> SomeNet n

instance (KnownNat i, KnownNat o) => B.Binary (OpaqueNet i o) where
    put oqn = case oqn of
                OpaqueNet n -> do
                  case n of
                    NetOL l -> do
                      B.put True
                      B.put l
                    NetIL (l :: RLayer i j) (n' :: Network j js o) -> do
                      B.put False
                      B.put $ natVal (Proxy :: Proxy j)
                      B.put l
                      B.put (OpaqueNet n')
    get = do
      isOL <- B.get
      if isOL
        then do
          OpaqueNet . NetOL <$> B.get
        else do
          j <- B.get
          reifyNat j $ \(Proxy :: Proxy j) -> do
            l   <- B.get :: B.Get (RLayer i j)
            nqo <- B.get :: B.Get (OpaqueNet j o)
            return $ case nqo of
              OpaqueNet n -> OpaqueNet $ l `NetIL` n


runFLayer :: (KnownNat i, KnownNat o) => FLayer i o -> R i -> R o
runFLayer (FLayer b w) v = b + w #> v
{-# INLINE runFLayer #-}

runRLayer :: (KnownNat i, KnownNat o)
          => (Double -> Double)
          -> RLayer i o
          -> R i
          -> (R o, RLayer i o)
runRLayer f l@(RLayer b wI wS s) v = (v', l { rLayerState = mapR f v' })
  where
    v'       = b + wI #> v + wS #> s
{-# INLINE runRLayer #-}

runNetwork :: forall i hs o. (KnownNat i, KnownNat o)
           => NeuralActs Double
           -> Network i hs o
           -> R i
           -> (R o, Network i hs o)
runNetwork (NA f g) = go
  where
    go :: forall i' hs'. KnownNat i'
       => Network i' hs' o
       -> R i'
       -> (R o, Network i' hs' o)
    go n v = case n of
               NetOL l    -> (mapR g (runFLayer l v), n)
               NetIL l nI -> let (v' , l')  = runRLayer f l v
                                 (v'', nI') = go nI (mapR f v')
                             in  (v'', NetIL l' nI')
{-# INLINE runNetwork #-}

runNetworkS :: (KnownNat i, KnownNat o, MonadState (Network i hs o) m)
            => NeuralActs Double
            -> R i
            -> m (R o)
runNetworkS na v = state (\n -> runNetwork na n v)
{-# INLINE runNetworkS #-}

runNetworkActs :: forall i hs o. (KnownNat i, KnownNat o)
               => NeuralActs Double
               -> Network i hs o
               -> R i
               -> (NetActs i hs o, Network i hs o)
runNetworkActs (NA f g) = go
  where
    go :: forall i' hs'. KnownNat i'
       => Network i' hs' o
       -> R i'
       -> (NetActs i' hs' o, Network i' hs' o)
    go n v = case n of
               NetOL l    -> (NetAOL (mapR g (runFLayer l v)), n)
               NetIL l nI -> let (v' , l') = runRLayer f l v
                                 vRes      = mapR f v'
                                 (nA, nI') = go nI vRes
                             in  (NetAIL vRes nA, NetIL l' nI')
{-# INLINE runNetworkActs #-}

runNetworkActsS :: (KnownNat i, KnownNat o, MonadState (Network i hs o) m)
                => NeuralActs Double
                -> R i
                -> m (NetActs i hs o)
runNetworkActsS na v = state (\n -> runNetworkActs na n v)
{-# INLINE runNetworkActsS #-}

runNetStream :: (KnownNat i, KnownNat o)
             => NeuralActs Double
             -> Network i hs o
             -> [R i]
             -> ([R o], Network i hs o)
runNetStream na n vs = runState (mapM (runNetworkS na) vs) n
{-# INLINE runNetStream #-}

runNetStream_ :: forall i hs o. (KnownNat i, KnownNat o)
              => NeuralActs Double
              -> Network i hs o
              -> [R i]
              -> [R o]
runNetStream_ na = go
  where
    go :: Network i hs o -> [R i] -> [R o]
    go n (v:vs) = let (u, n') = runNetwork na n v
                  in  u `deepseq` n' `deepseq` u : go n' vs
    go _ []     = []
{-# INLINE runNetStream_ #-}

runNetStreamActs :: (KnownNat i, KnownNat o)
                 => NeuralActs Double
                 -> Network i hs o
                 -> [R i]
                 -> ([NetActs i hs o], Network i hs o)
runNetStreamActs na n vs = runState (mapM (runNetworkActsS na) vs) n
{-# INLINE runNetStreamActs #-}

runNetStreamActs_ :: forall i hs o. (KnownNat i, KnownNat o)
                  => NeuralActs Double
                  -> Network i hs o
                  -> [R i]
                  -> [NetActs i hs o]
runNetStreamActs_ na = go
  where
    go :: Network i hs o -> [R i] -> [NetActs i hs o]
    go n (v:vs) = let (u, n') = runNetworkActs na n v
                  in  u `deepseq` n' `deepseq` u : go n' vs
    go _ []     = []
{-# INLINE runNetStreamActs_ #-}



