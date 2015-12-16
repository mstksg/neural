{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Neural.Recurrent.Modify where

import Control.Applicative
import Control.Lens hiding    (Index, ix)
import Data.Finite
import Data.Neural.Recurrent
import Data.Neural.Types
import Data.Neural.Utility
import Data.Proxy
import Data.Type.Fin
import Data.Type.Index
import GHC.TypeLits           as L
import GHC.TypeLits.Witnesses
import Linear.V
import Type.Family.Nat hiding (type (+))
import qualified Data.Vector  as V

deleteLayer :: KnownNet i hs o
            => (forall j k. (KnownNat j, KnownNat k) => Node j a -> Node k a)
            -> (forall j k l. (KnownNat j, KnownNat k, KnownNat l) => RNode j l a -> RNode k l a)
            -> Fin (Len hs)
            -> Network i hs o a
            -> OpaqueNet i o a
deleteLayer mO mI fn nt =
    case nt of
      NetIL l nt' ->
        case fn of
          FZ ->
            case nt' of
              NetOL l'      -> OpaqueNet . NetOL          $ over (tFLayerNodes . mapped) mO l'
              NetIL l' nt'' -> OpaqueNet . (`NetIL` nt'') $ over (tRLayerNodes . mapped) mI l'
          FS fn' -> case deleteLayer mO mI fn' nt' of
                      OpaqueNet ntD -> OpaqueNet $ l `NetIL` ntD
      NetOL _ -> error "impossible!"

addLayer :: (KnownNet i hs o, KnownNat l)
         => (forall j k. (KnownNat j, KnownNat k) => (RLayer j l a, FLayer l k a))
         -> (forall j k. (KnownNat j, KnownNat k) => (RLayer j l a, RLayer l k a))
         -> Fin (Len (i ': hs))
         -> Network i hs o a
         -> (OpaqueNet i o a)
addLayer mO mI fn nt =
    case fn of
      FZ ->
        case nt of
          NetOL _     -> let (li, lo) = mO
                         in  OpaqueNet $ li `NetIL` NetOL lo
          NetIL _ nt' -> let (li, lo) = mI
                         in  OpaqueNet $ li `NetIL` lo `NetIL` nt'
      FS fn' ->
        case nt of
          NetIL l nt' ->
            case addLayer mO mI fn' nt' of
              OpaqueNet ntA -> OpaqueNet $ l `NetIL` ntA
          NetOL _ -> error "impossible!"

deleteNode :: forall i hs h o a. KnownNet i hs o
           => Index hs h
           -> Finite h
           -> Network i hs o a
           -> OpaqueNet i o a
deleteNode ix fn nt =
    case ix of
      IZ ->
        case nt of
          NetIL (l :: RLayer i h a) nt' ->
            withNatOp (%-) (Proxy :: Proxy h) (Proxy :: Proxy 1) $
              let lD :: RLayer i (h - 1) a
                  lD = RLayer (over (traverse . tRNodeSWeights) (deleteV fn)
                                . deleteV fn
                                $ rLayerNodes l)
                              (deleteV fn $ rLayerState l)
              in  case nt' of
                    NetOL l' ->
                      let l'D :: FLayer (h - 1) o a
                          l'D = over (tFLayerNodes . traverse . tNodeWeights) (deleteV fn) l'
                      in  OpaqueNet $ lD `NetIL` NetOL l'D
                    NetIL (l' :: RLayer h j a) nt' ->
                      let l'D :: RLayer (h - 1) j a
                          l'D = RLayer (over (traverse . tRNodeIWeights) (deleteV fn)
                                         $ rLayerNodes l' )
                                       (rLayerState l')
                      in  OpaqueNet $ lD `NetIL` l'D `NetIL` nt'
          _ -> error "impossible!"
      IS ix' ->
        case nt of
          NetIL l nt' ->
            case deleteNode ix' fn nt' of
              OpaqueNet ntD -> OpaqueNet $ l `NetIL` ntD
          _ -> error "impossible!"

addNode :: forall i hs o a f. (Applicative f, KnownNet i hs o)
        => (forall j l. (KnownNat j, KnownNat l) => f (RNode j l a, V l a, a, V o a))
        -> Int
        -> Fin (Len hs)
        -> Network i hs o a
        -> f (OpaqueNet i o a)
addNode mO mI fn nt =
    case nt of
      NetIL (l :: RLayer i j a) nt' ->
        case fn of
          FZ -> withNatOp (%+) (Proxy :: Proxy j) (Proxy :: Proxy 1) $
            case nt' of
              NetOL l' -> flip fmap mO $ \(lNew, wsNew, sNew, l'New) ->
                            let lA :: RLayer i (j + 1) a
                                -- lA = RLayer ((`snocV` lNew) . liftA2 undefined wsNew $ rLayerNodes l)
                                lA = RLayer ((`snocV` lNew) . undefined $ rLayerNodes l)
                                            (rLayerState l `snocV` sNew)
                                l'A :: FLayer (j + 1) o a
                                l'A = FLayer $ liftA2 (\w -> over tNodeWeights (`snocV` w)) l'New $ layerNodes l'
                            in  OpaqueNet $ lA `NetIL` NetOL l'A
              NetIL l' nt'' -> undefined
          FS fn' ->
            flip fmap (addNode mO mI fn' nt') $
              \o -> case o of
                      OpaqueNet ntA -> OpaqueNet $ l `NetIL` ntA
      NetOL _ -> error "impossible!"
