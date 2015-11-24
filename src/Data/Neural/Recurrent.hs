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

-- import Control.DeepSeq
-- import Control.Monad.Trans.State
-- import Data.Neural.Types
-- import Data.Neural.Utility
-- import Data.Proxy
-- import Control.Lens
-- import Control.Arrow
-- import GHC.Generics
-- import GHC.TypeLits
-- import Linear.V
-- import System.Random
-- import Type.Class.Witness
-- import qualified Data.Binary     as B
-- import qualified Data.Vector     as V

-- data RLayer :: Nat -> Nat -> * -> * where
--     RLayer :: { rLayerLayer :: !(Layer (i + o) o a)
--               , rLayerState :: !(V o a)
--               } -> RLayer i o a
--   deriving (Show, Functor, Foldable, Traversable, Generic)

-- instance NFData a => NFData (RLayer i o a)

-- data Network :: Nat -> [Nat] -> Nat -> * -> * where
--     OLayer :: !(RLayer i o a) -> Network i '[] o a
--     ILayer :: KnownNat j => !(RLayer i j a) -> !(Network j hs o a) -> Network i (j ': hs) o a

-- infixr 5 `ILayer`

-- deriving instance Functor (Network i hs o)

-- instance (KnownNat i, KnownNat o) => Applicative (RLayer i o) where
--     pure x =
--       case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
--         Wit -> RLayer (pure x) (pure x)
--     {-# INLINE pure #-}
--     RLayer l s <*> RLayer l' s' =
--       case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
--         Wit -> RLayer (l <*> l') (s <*> s')
--     {-# INLINE (<*>) #-}

-- instance (B.Binary a, KnownNat i, KnownNat o) => B.Binary (RLayer i o a) where
--     put (RLayer l s) =
--       case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
--         Wit -> B.put l *> B.put s
--     get =
--       case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
--         Wit -> RLayer <$> B.get <*> B.get

-- instance (KnownNat i, KnownNat o, Random a) => Random (RLayer i o a) where
--     random g =
--       case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
--         Wit -> flip runState g $
--                  RLayer <$> state random <*> state random
--     randomR (RLayer l s, RLayer l' s') g =
--       case natAddition (Proxy :: Proxy i) (Proxy :: Proxy o) of
--         Wit -> flip runState g $
--                  RLayer <$> state (randomR (l, l'))
--                         <*> state (randomR (s, s'))

-- instance (KnownNat i, KnownNat o) => Applicative (Network i '[] o) where
--     pure = OLayer . pure
--     {-# INLINE pure #-}
--     OLayer f <*> OLayer x = OLayer (f <*> x)
--     {-# INLINE (<*>) #-}

-- instance (KnownNat i, KnownNat o, KnownNat j, Applicative (Network j hs o)) => Applicative (Network i (j ': hs) o) where
--     pure x = pure x `ILayer` pure x
--     {-# INLINE pure #-}
--     ILayer fi fr <*> ILayer xi xr = ILayer (fi <*> xi) (fr <*> xr)
--     {-# INLINE (<*>) #-}

-- instance (KnownNat i, KnownNat o, Random a) => Random (Network i '[] o a) where
--     random = first OLayer . random
--     randomR (OLayer rmn, OLayer rmx) = first OLayer . randomR (rmn, rmx)

-- instance (KnownNat i, KnownNat o, KnownNat j, Random a, Random (Network j hs o a)) => Random (Network i (j ': hs) o a) where
--     random g = let (l, g') = random g
--                in  first (l `ILayer`) (random g')
--     randomR (ILayer lmn nmn, ILayer lmx nmx) g =
--         let (l , g') = randomR (lmn, lmx) g
--         in  first (l `ILayer`) (randomR (nmn, nmx) g')

-- instance (KnownNat i, KnownNat o, B.Binary a) => B.Binary (Network i '[] o a) where
--     put (OLayer l) = B.put l
--     get = OLayer <$> B.get

-- instance (KnownNat i, KnownNat o, KnownNat j, B.Binary a, B.Binary (Network j hs o a)) => B.Binary (Network i (j ': hs) o a) where
--     put (ILayer l n') = B.put l *> B.put n'
--     get = ILayer <$> B.get <*> B.get

-- instance NFData a => NFData (Network i hs o a) where
--     rnf (OLayer (force -> !l)) = ()
--     rnf (ILayer (force -> !l) (force -> !n)) = ()

-- deriving instance Show a => Show (Network i hs o a)
-- deriving instance Foldable (Network i hs o)
-- deriving instance Traversable (Network i hs o)


-- -- -- TODO: extend to multiple layers?
-- -- data Network :: Nat -> Nat -> Nat -> * -> * where
-- --     Network :: { _inputLayer  :: !(Layer (i + h) h a)
-- --                , _hiddenState :: !(V h a)
-- --                , _outLayer    :: !(Layer h       o a)
-- --                } -> Network i h o a
-- --     deriving (Generic)

-- -- instance Functor (Network i h o) where
-- --     fmap f (Network iL hS oL) = Network (fmap f iL)
-- --                                         (fmap f hS)
-- --                                         (fmap f oL)
-- --     {-# INLINE fmap #-}

-- -- instance (KnownNat i, KnownNat o, KnownNat h) => Applicative (Network i h o) where
-- --     pure x =
-- --       case natAddition (Proxy :: Proxy i) (Proxy :: Proxy h) of
-- --         Wit -> Network (pure x) (pure x) (pure x)
-- --     {-# INLINE pure #-}
-- --     Network iL hS oL <*> Network iL' hS' oL' =
-- --       case natAddition (Proxy :: Proxy i) (Proxy :: Proxy h) of
-- --         Wit -> Network (iL <*> iL') (hS <*> hS') (oL <*> oL')
-- --     {-# INLINE (<*>) #-}

-- -- instance (B.Binary a, KnownNat i, KnownNat h, KnownNat o) => B.Binary (Network i h o a) where
-- --     put (Network iL hS oL) =
-- --       case natAddition (Proxy :: Proxy i) (Proxy :: Proxy h) of
-- --         Wit -> B.put iL
-- --             *> B.put hS
-- --             *> B.put oL
-- --     get =
-- --       case natAddition (Proxy :: Proxy i) (Proxy :: Proxy h) of
-- --         Wit -> Network <$> B.get <*> B.get <*> B.get

-- -- instance (KnownNat i, KnownNat h, KnownNat o, Random a) => Random (Network i h o a) where
-- --     random g =
-- --       case natAddition (Proxy :: Proxy i) (Proxy :: Proxy h) of
-- --         Wit -> flip runState g $
-- --                  Network <$> state random <*> state random <*> state random
-- --     randomR (Network mniL mnhS mnoL, Network mxiL mxhS mxoL) g =
-- --       case natAddition (Proxy :: Proxy i) (Proxy :: Proxy h) of
-- --         Wit -> flip runState g $
-- --                  Network <$> state (randomR (mniL, mxiL))
-- --                          <*> state (randomR (mnhS, mxhS))
-- --                          <*> state (randomR (mnoL, mxoL))

-- -- deriving instance Show a => Show (Network i h o a)
-- -- deriving instance Foldable (Network i h o)
-- -- deriving instance Traversable (Network i h o)
-- -- instance NFData a => NFData (Network i h o a)

-- -- -- some lenses
-- -- hiddenState :: Lens' (Network i h o a) (V h a)
-- -- hiddenState f n = (\hS -> n { _hiddenState = hS }) <$> f (_hiddenState n)
-- -- {-# INLINE hiddenState #-}

-- -- inputLayer :: Lens (Network i h o a  ) (Network i' h o a  )
-- --                    (Layer (i + h) h a) (Layer (i' + h) h a)
-- -- inputLayer f n = (\iL -> n { _inputLayer = iL }) <$> f (_inputLayer n)
-- -- {-# INLINE inputLayer #-}

-- -- outLayer :: Lens (Network i h o a) (Network i h o' a)
-- --                  (Layer     h o a) (Layer     h o' a)
-- -- outLayer f n = (\oL -> n { _outLayer = oL }) <$> f (_outLayer n)
-- -- {-# INLINE outLayer #-}


-- -- runNetwork :: forall i h o a. (Num a, KnownNat i, KnownNat h)
-- --            => (a -> a) -> (a -> a)
-- --            -> V i a
-- --            -> Network i h o a
-- --            -> (V o a, Network i h o a)
-- -- runNetwork f g v n =
-- --     case natAddition (Proxy :: Proxy i) (Proxy :: Proxy h) of
-- --       Wit -> go
-- --   where
-- --     go :: KnownNat (i + h) => (V o a, Network i h o a)
-- --     go = (v', n')
-- --       where
-- --         -- not verified by compiler.  beware!
-- --         -- also, is concatting to slow?  should hS be found by directly
-- --         -- zipping?
-- --         vInp :: V (i + h) a
-- --         vInp = V (toVector v V.++ toVector (_hiddenState n))
-- --         hS :: V h a
-- --         hS = f <$> runLayer (_inputLayer n) vInp
-- --         v' :: V o a
-- --         v' = g <$> runLayer (_outLayer n) hS
-- --         n' :: Network i h o a
-- --         n' = n { _hiddenState = hS }

-- -- runNetworkS :: (Num a, KnownNat i, KnownNat h)
-- --             => (a -> a) -> (a -> a)
-- --             -> V i a
-- --             -> State (Network i h o a) (V o a)
-- -- runNetworkS f g v = state (runNetwork f g v)

-- -- runNetStream :: forall i h o a. (Num a, KnownNat i, KnownNat h)
-- --              => (a -> a) -> (a -> a)
-- --              -> Network i h o a
-- --              -> [V i a]
-- --              -> ([V o a], Network i h o a)
-- -- runNetStream f g n vs = runState (mapM (runNetworkS f g) vs) n

-- -- runNetStream_ :: forall i h o a. (Num a, KnownNat i, KnownNat h)
-- --               => (a -> a) -> (a -> a)
-- --               -> Network i h o a
-- --               -> [V i a]
-- --               -> [V o a]
-- -- runNetStream_ f g = go
-- --   where
-- --     go :: Network i h o a -> [V i a] -> [V o a]
-- --     go n (v:vs) = let (u, n') = runNetwork f g v n
-- --                   in  u : go n' vs

-- -- runNetFeedback :: forall i h a. (Num a, KnownNat i, KnownNat h)
-- --                => (a -> a) -> (a -> a)
-- --                -> Network i h i a
-- --                -> V i a
-- --                -> [(V i a, Network i h i a)]
-- -- runNetFeedback f g = go
-- --   where
-- --     go :: Network i h i a -> V i a -> [(V i a, Network i h i a)]
-- --     go n v = let res@(v', n') = runNetwork f g v n
-- --              in  res : go n' v'

-- -- runNetFeedback_ :: forall i h a. (Num a, KnownNat i, KnownNat h)
-- --                 => (a -> a) -> (a -> a)
-- --                 -> Network i h i a
-- --                 -> V i a
-- --                 -> [V i a]
-- -- runNetFeedback_ f g = go
-- --   where
-- --     go :: Network i h i a -> V i a -> [V i a]
-- --     go n v = let (v', n') = runNetwork f g v n
-- --              in  v' : go n' v'

-- -- randomNetwork :: (RandomGen g, Random (Network i h o a), Num a)
-- --               => g
-- --               -> (Network i h o a, g)
-- -- randomNetwork g = first (fmap (subtract 1 . (*2))) $ random g

-- -- randomNetwork' :: (KnownNat h, RandomGen g, Random (Network i h o a), Num a)
-- --                => g
-- --                -> (Network i h o a, g)
-- -- randomNetwork' = first resetNetState . randomNetwork

-- -- randomNetworkIO :: (Random (Network i h o a), Num a) => IO (Network i h o a)
-- -- randomNetworkIO = fmap (subtract 1 . (*2)) <$> randomIO

-- -- randomNetworkIO' :: (KnownNat h, Random (Network i h o a), Num a) => IO (Network i h o a)
-- -- randomNetworkIO' = resetNetState <$> randomNetworkIO

-- -- resetNetState :: (Num a, KnownNat h) => Network i h o a -> Network i h o a
-- -- -- resetNetState = set hiddenState (pure 0)
-- -- resetNetState n = n { _hiddenState = pure 0 }
-- -- {-# INLINE resetNetState #-}
