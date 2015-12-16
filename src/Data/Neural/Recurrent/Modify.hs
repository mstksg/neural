{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Neural.Recurrent.Modify where

import Data.Type.Fin
import GHC.TypeLits
import Data.Neural.Recurrent
import Data.Neural.Types
import Type.Family.Nat

deleteLayer :: (Applicative f, KnownNet i hs o)
            => (forall j k. Node j a -> f (Node k a))
            -> (forall j k l. RNode j l a -> f (RNode k l a))
            -> Fin (Len hs)
            -> Network i hs o a
            -> f (OpaqueNet i o a)
deleteLayer mO mI fn nt =
    case nt of
      NetIL l nt' ->
        case fn of
          FZ ->
            case nt' of
              NetOL l'      -> OpaqueNet . NetOL          <$> (tFLayerNodes . traverse) mO l'
              NetIL l' nt'' -> OpaqueNet . (`NetIL` nt'') <$> (tRLayerNodes . traverse) mI l'
          FS fn' ->
            flip fmap (deleteLayer mO mI fn' nt') $
              \o -> case o of
                      OpaqueNet ntD -> OpaqueNet $ l `NetIL` ntD
      NetOL _ -> error "impossible!"

addLayer :: (Applicative f, KnownNet i hs o, KnownNat l)
         => (forall j k. f (RLayer j l a, FLayer l k a))
         -> (forall j k. f (RLayer j l a, RLayer l k a))
         -> Fin (Len (i ': hs))
         -> Network i hs o a
         -> f (OpaqueNet i o a)
addLayer mO mI fn nt =
    case fn of
      FZ ->
        case nt of
          NetOL _     -> flip fmap mO $ \(li, lo) -> OpaqueNet $ li `NetIL` NetOL lo
          NetIL _ nt' -> flip fmap mI $ \(li, lo) -> OpaqueNet $ li `NetIL` lo `NetIL` nt'
      FS fn' ->
        case nt of
          NetIL l nt' ->
            flip fmap (addLayer mO mI fn' nt') $
              \o -> case o of
                      OpaqueNet ntA -> OpaqueNet $ l `NetIL` ntA
          NetOL _ -> error "impossible!"

