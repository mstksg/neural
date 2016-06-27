{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Neural.Recurrent.Dropout where

-- import Control.Monad
-- import Data.Bifunctor
-- import GHC.TypeLits
import Control.Lens
import Data.Neural.Recurrent
import Data.Neural.Types
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
        let l'  = compensateSWeights l
            -- n'' = runIdentity . tNetRLayers (Identity . compensateStates . compensateWeights) $ n'
            n'' = runIdentity . tNetRLayers (Identity . compensateWeights) $ n'
        in  NetIL l' n''
  where
    compensateWeights :: forall j k. RLayer j k a -> RLayer j k a
    compensateWeights = over (tRLayerNodes . traverse . tRNodeWeights) (bimap ((1 - d) *^) ((1 - d) *^))
    compensateSWeights :: forall j k. RLayer j k a -> RLayer j k a
    compensateSWeights = over (tRLayerNodes . traverse . tRNodeSWeights) ((1 - d) *^)
    -- compensateStates :: forall j k. (KnownNat j, KnownNat k) => RLayer j k a -> RLayer j k a
    -- compensateStates = over tRLayerState ((1 - d) *^)
