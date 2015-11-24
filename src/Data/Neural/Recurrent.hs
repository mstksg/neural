{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}

module Data.Neural.Recurrent where

import Control.DeepSeq
import Control.Monad.State
import Data.Neural.Types hiding (Network)
import qualified Data.Neural.Types as N
import Data.Neural.Utility
import Data.Proxy
import Control.Lens
import Control.Arrow
import GHC.Generics
import GHC.TypeLits
import Linear.V
import System.Random
import Type.Class.Witness
import qualified Data.Binary     as B
import qualified Data.Vector     as V

data RLayer :: Nat -> Nat -> * -> * where
    RLayer :: { rLayerWeights :: !(FLayer (i + o) o a)
              , rLayerState   :: !(V o a)
              } -> RLayer i o a
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (RLayer i o a)

type Network = N.Network RLayer

instance (KnownNat i, KnownNat o) => Applicative (RLayer i o) where
    pure x =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
        Wit -> RLayer (pure x) (pure x)
    {-# INLINE pure #-}
    RLayer l s <*> RLayer l' s' =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
        Wit -> RLayer (l <*> l') (s <*> s')
    {-# INLINE (<*>) #-}

instance (B.Binary a, KnownNat i, KnownNat o) => B.Binary (RLayer i o a) where
    put (RLayer l s) =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
        Wit -> B.put l *> B.put s
    get =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
        Wit -> RLayer <$> B.get <*> B.get

instance (KnownNat i, KnownNat o, Random a) => Random (RLayer i o a) where
    random g =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
        Wit -> flip runState g $
                 RLayer <$> state random <*> state random
    randomR (RLayer l s, RLayer l' s') g =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
        Wit -> flip runState g $
                 RLayer <$> state (randomR (l, l'))
                        <*> state (randomR (s, s'))

runRLayer :: forall i o a. (KnownNat i, KnownNat o, Num a)
          => (a -> a)
          -> RLayer i o a
          -> V i a
          -> (V o a, RLayer i o a)
runRLayer f l v =
    case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
      Wit -> go
  where
    go :: KnownNat (i + o) => (V o a, RLayer i o a)
    go = (v', l { rLayerState = newState })
      where
        -- not verified by compiler.  beware!
        -- also, is concatting to slow?
        vInp :: V (i + o) a
        vInp = V (toVector v V.++ toVector (rLayerState l))
        v' :: V o a
        v' = runFLayer (rLayerWeights l) vInp
        newState :: V o a
        newState = f <$> v'

runRLayerS :: forall i o a m. (KnownNat i, KnownNat o, Num a, MonadState (RLayer i o a) m)
           => (a -> a)
           -> V i a
           -> m (V o a)
runRLayerS f v = state (\l -> runRLayer f l v)

runNetwork :: forall i hs o a. (Num a, KnownNat i, KnownNat o)
           => (a -> a) -> (a -> a)
           -> Network i hs o a
           -> V i a
           -> (V o a, Network i hs o a)
runNetwork f g = go
  where
    go :: forall i' hs' o'. (KnownNat i', KnownNat o') => Network i' hs' o' a -> V i' a -> (V o' a, Network i' hs' o' a)
    go n v = case n of
               NetOL l    -> (fmap g *** NetOL) (runRLayer f l v)
               NetIL l nI -> let (v' , l')  = runRLayer f l v
                                 (v'', nI') = go nI (f <$> v')
                             in  (v'', NetIL l' nI')

runNetworkS :: (Num a, KnownNat i, KnownNat o, MonadState (Network i hs o a) m)
            => (a -> a) -> (a -> a)
            -> V i a
            -> m (V o a)
runNetworkS f g v = state (\n -> runNetwork f g n v)

runNetStream :: forall i hs o a. (Num a, KnownNat i, KnownNat o)
             => (a -> a) -> (a -> a)
             -> Network i hs o a
             -> [V i a]
             -> ([V o a], Network i hs o a)
runNetStream f g n vs = runState (mapM (runNetworkS f g) vs) n

runNetStream_ :: forall i hs o a. (Num a, KnownNat i, KnownNat o)
              => (a -> a) -> (a -> a)
              -> Network i hs o a
              -> [V i a]
              -> [V o a]
runNetStream_ f g = go
  where
    go :: Network i hs o a -> [V i a] -> [V o a]
    go n (v:vs) = let (u, n') = runNetwork f g n v
                  in  u : go n' vs

runNetFeedback :: forall i hs a. (Num a, KnownNat i)
               => (a -> a) -> (a -> a)
               -> Network i hs i a
               -> V i a
               -> [(V i a, Network i hs i a)]
runNetFeedback f g = go
  where
    go :: Network i hs i a -> V i a -> [(V i a, Network i hs i a)]
    go n v = let res@(v', n') = runNetwork f g n v
             in  res : go n' v'

runNetFeedback_ :: forall i hs a. (Num a, KnownNat i)
                => (a -> a) -> (a -> a)
                -> Network i hs i a
                -> V i a
                -> [V i a]
runNetFeedback_ f g = go
  where
    go :: Network i hs i a -> V i a -> [V i a]
    go n v = let (v', n') = runNetwork f g n v
             in  v' : go n' v'

randomNetwork :: (RandomGen g, Random (Network i hs o a), Num a)
              => g
              -> (Network i hs o a, g)
randomNetwork g = first (fmap (subtract 1 . (*2))) $ random g

-- randomNetwork' :: (KnownNat h, RandomGen g, Random (Network i h o a), Num a)
--                => g
--                -> (Network i h o a, g)
-- randomNetwork' = first resetNetState . randomNetwork

randomNetworkIO :: (Random (Network i hs o a), Num a) => IO (Network i hs o a)
randomNetworkIO = fmap (subtract 1 . (*2)) <$> randomIO

-- -- randomNetworkIO' :: (KnownNat h, Random (Network i h o a), Num a) => IO (Network i h o a)
-- -- randomNetworkIO' = resetNetState <$> randomNetworkIO

-- -- resetNetState :: (Num a, KnownNat h) => Network i h o a -> Network i h o a
-- -- -- resetNetState = set hiddenState (pure 0)
-- -- resetNetState n = n { _hiddenState = pure 0 }
-- -- {-# INLINE resetNetState #-}

-- | Boilerplate instances

instance Functor (Network i hs o) where
    fmap f n = case n of
                 NetOL l -> NetOL (fmap f l)
                 NetIL l n' -> fmap f l `NetIL` fmap f n'
    {-# INLINE fmap #-}

instance (KnownNat i, KnownNat o) => Applicative (Network i '[] o) where
    pure = NetOL . pure
    {-# INLINE pure #-}
    NetOL f <*> NetOL x = NetOL (f <*> x)
    {-# INLINE (<*>) #-}

instance (KnownNat i, KnownNat o, KnownNat j, Applicative (Network j hs o)) => Applicative (Network i (j ': hs) o) where
    pure x = pure x `NetIL` pure x
    {-# INLINE pure #-}
    NetIL fi fr <*> NetIL xi xr = NetIL (fi <*> xi) (fr <*> xr)
    {-# INLINE (<*>) #-}

instance (KnownNat i, KnownNat o, Random a) => Random (Network i '[] o a) where
    random = first NetOL . random
    randomR (NetOL rmn, NetOL rmx) = first NetOL . randomR (rmn, rmx)

instance (KnownNat i, KnownNat o, KnownNat j, Random a, Random (Network j hs o a)) => Random (Network i (j ': hs) o a) where
    random g = let (l, g') = random g
               in  first (l `NetIL`) (random g')
    randomR (NetIL lmn nmn, NetIL lmx nmx) g =
        let (l , g') = randomR (lmn, lmx) g
        in  first (l `NetIL`) (randomR (nmn, nmx) g')

instance (KnownNat i, KnownNat o, B.Binary a) => B.Binary (Network i '[] o a) where
    put (NetOL l) = B.put l
    get = NetOL <$> B.get

instance (KnownNat i, KnownNat o, KnownNat j, B.Binary a, B.Binary (Network j hs o a)) => B.Binary (Network i (j ': hs) o a) where
    put (NetIL l n') = B.put l *> B.put n'
    get = NetIL <$> B.get <*> B.get

instance NFData a => NFData (Network i hs o a) where
    rnf (NetOL (force -> !l)) = ()
    rnf (NetIL (force -> !l) (force -> !n)) = ()

deriving instance Show a => Show (Network i hs o a)
deriving instance Foldable (Network i hs o)
deriving instance Traversable (Network i hs o)
