{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Data.Neural.Recurrent where

import Control.DeepSeq
import Control.Monad.Trans.State
import Data.Neural.Types
import Data.Neural.Utility
import Data.Proxy
import Control.Arrow
import GHC.Generics
import GHC.TypeLits
import Linear.V
import System.Random
import Type.Class.Witness
import qualified Data.Binary     as B
import qualified Data.Vector     as V

data Network :: Nat -> Nat -> Nat -> * -> * where
    Network :: { inputLayer  :: !(Layer (i + h) h a)
               , hiddenState :: !(V h a)
               , outLayer    :: !(Layer h       o a)
               } -> Network i h o a
    deriving (Generic)

instance Functor (Network i h o) where
    fmap f (Network iL hS oL) = Network (fmap f iL)
                                        (fmap f hS)
                                        (fmap f oL)
    {-# INLINE fmap #-}

instance (KnownNat i, KnownNat o, KnownNat h) => Applicative (Network i h o) where
    pure x =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy h) of
        Wit -> Network (pure x) (pure x) (pure x)
    {-# INLINE pure #-}
    Network iL hS oL <*> Network iL' hS' oL' =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy h) of
        Wit -> Network (iL <*> iL') (hS <*> hS') (oL <*> oL')
    {-# INLINE (<*>) #-}

instance (B.Binary a, KnownNat i, KnownNat h, KnownNat o) => B.Binary (Network i h o a) where
    put (Network iL hS oL) =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy h) of
        Wit -> B.put iL
            *> B.put hS
            *> B.put oL
    get =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy h) of
        Wit -> Network <$> B.get <*> B.get <*> B.get

instance (KnownNat i, KnownNat h, KnownNat o, Random a) => Random (Network i h o a) where
    random g =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy h) of
        Wit -> flip runState g $
                 Network <$> state random <*> state random <*> state random
    randomR (Network mniL mnhS mnoL, Network mxiL mxhS mxoL) g =
      case natAddition (Proxy :: Proxy i) (Proxy :: Proxy h) of
        Wit -> flip runState g $
                 Network <$> state (randomR (mniL, mxiL))
                         <*> state (randomR (mnhS, mxhS))
                         <*> state (randomR (mnoL, mxoL))

deriving instance Show a => Show (Network i h o a)
deriving instance Foldable (Network i h o)
deriving instance Traversable (Network i h o)
instance NFData a => NFData (Network i h o a)

runNetwork :: forall i h o a. (Num a, KnownNat i, KnownNat h)
           => (a -> a) -> (a -> a)
           -> V i a
           -> Network i h o a
           -> (V o a, Network i h o a)
runNetwork f g v n =
    case natAddition (Proxy :: Proxy i) (Proxy :: Proxy h) of
      Wit -> go
  where
    go :: KnownNat (i + h) => (V o a, Network i h o a)
    go = (v', n')
      where
        -- not verified by compiler.  beware!
        -- also, is concatting to slow?  should hS be found by directly
        -- zipping?
        vInp :: V (i + h) a
        vInp = V (toVector v V.++ toVector (hiddenState n))
        hS :: V h a
        hS = f <$> runLayer (inputLayer n) vInp
        v' :: V o a
        v' = g <$> runLayer (outLayer n) hS
        n' :: Network i h o a
        n' = n { hiddenState = hS }

runNetworkS :: (Num a, KnownNat i, KnownNat h)
            => (a -> a) -> (a -> a)
            -> V i a
            -> State (Network i h o a) (V o a)
runNetworkS f g v = state (runNetwork f g v)

runNetStream :: forall i h o a. (Num a, KnownNat i, KnownNat h)
             => (a -> a) -> (a -> a)
             -> Network i h o a
             -> [V i a]
             -> ([V o a], Network i h o a)
runNetStream f g n vs = runState (mapM (runNetworkS f g) vs) n

runNetStream_ :: forall i h o a. (Num a, KnownNat i, KnownNat h)
              => (a -> a) -> (a -> a)
              -> Network i h o a
              -> [V i a]
              -> [V o a]
runNetStream_ f g = go
  where
    go :: Network i h o a -> [V i a] -> [V o a]
    go n (v:vs) = let (u, n') = runNetwork f g v n
                  in  u : go n' vs

runNetFeedback :: forall i h a. (Num a, KnownNat i, KnownNat h)
               => (a -> a) -> (a -> a)
               -> Network i h i a
               -> V i a
               -> [(V i a, Network i h i a)]
runNetFeedback f g = go
  where
    go :: Network i h i a -> V i a -> [(V i a, Network i h i a)]
    go n v = let res@(v', n') = runNetwork f g v n
             in  res : go n' v'

runNetFeedback_ :: forall i h a. (Num a, KnownNat i, KnownNat h)
                => (a -> a) -> (a -> a)
                -> Network i h i a
                -> V i a
                -> [V i a]
runNetFeedback_ f g = go
  where
    go :: Network i h i a -> V i a -> [V i a]
    go n v = let (v', n') = runNetwork f g v n
             in  v' : go n' v'

randomNetwork :: (RandomGen g, Random (Network i h o a), Num a)
              => g
              -> (Network i h o a, g)
randomNetwork g = (first . fmap) (subtract 1 . (*2)) $ random g

randomNetworkIO :: (Random (Network i h o a), Num a) => IO (Network i h o a)
randomNetworkIO = fmap (subtract 1 . (*2)) <$> randomIO
