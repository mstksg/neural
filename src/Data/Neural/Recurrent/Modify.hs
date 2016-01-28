{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

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
         -> OpaqueNet i o a
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
                    NetIL (l' :: RLayer h j a) nt'' ->
                      let l'D :: RLayer (h - 1) j a
                          l'D = RLayer (over (traverse . tRNodeIWeights) (deleteV fn)
                                         $ rLayerNodes l' )
                                       (rLayerState l')
                      in  OpaqueNet $ lD `NetIL` l'D `NetIL` nt''
          _ -> error "impossible!"
      IS ix' ->
        case nt of
          NetIL l nt' ->
            case deleteNode ix' fn nt' of
              OpaqueNet ntD -> OpaqueNet $ l `NetIL` ntD
          _ -> error "impossible!"

addNode :: forall i hs o a. KnownNet i hs o
        => (forall j l. (KnownNat j, KnownNat l, KnownNat (l + 1)) => (RNode j (l + 1) a, V l a, a))
        -> (forall k. KnownNat k => V k a)
        -> Fin (Len hs)
        -> Network i hs o a
        -> OpaqueNet i o a
addNode mInp mOut fn nt =
    case nt of
      NetIL (l :: RLayer i j a) nt' ->
        case fn of
          FZ -> withNatOp (%+) (Proxy :: Proxy j) (Proxy :: Proxy 1) $
            let (lNew, wsNew, sNew) = mInp
                lA :: RLayer i (j + 1) a
                lA = RLayer ((`snocV` lNew)
                              . liftA2 (\w -> over tRNodeSWeights (`snocV` w)) wsNew
                              $ rLayerNodes l)
                            (rLayerState l `snocV` sNew)
            in  case nt' of
                  NetOL l' ->
                    let l'A :: FLayer (j + 1) o a
                        l'A = FLayer $ liftA2 (\w -> over tNodeWeights (`snocV` w)) mOut $ layerNodes l'
                    in  OpaqueNet $ lA `NetIL` NetOL l'A
                  NetIL (l' :: RLayer j k a) nt'' ->
                    let l'A :: RLayer (j + 1) k a
                        l'A = RLayer (liftA2 (\w -> over tRNodeIWeights (`snocV` w)) mOut $ rLayerNodes l')
                                     (rLayerState l')
                    in  OpaqueNet $ lA `NetIL` l'A `NetIL` nt''
          FS fn' -> case addNode mInp mOut fn' nt' of
                      OpaqueNet ntA -> OpaqueNet $ l `NetIL` ntA
      NetOL _ -> error "impossible!"
