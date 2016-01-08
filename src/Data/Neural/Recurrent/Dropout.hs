{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Neural.Recurrent.Dropout where

import Control.Lens
import Data.Neural.Recurrent
import Data.Neural.Types
import GHC.TypeLits
import Linear

compensateDO
    :: forall i hs o a. (Num a, KnownNet i hs o)
    => a
    -> Network i hs o a
    -> Network i hs o a
compensateDO d n =
    case n of
      NetOL _ -> n
      NetIL l n' ->
        let l'  = compensateStates l
            n'' = runIdentity . tNetRLayers (Identity . compensateStates . compensateInps) $ n'
        in  NetIL l' n''
  where
    compensateStates :: forall j k. (KnownNat j, KnownNat k) => RLayer j k a -> RLayer j k a
    compensateStates = over (tRLayerNodes . traverse . tRNodeSWeights) ((1-d) *^)
    compensateInps   :: forall j k. (KnownNat j, KnownNat k) => RLayer j k a -> RLayer j k a
    compensateInps   = over (tRLayerNodes . traverse . tRNodeIWeights) ((1-d) *^)
