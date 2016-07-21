{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
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
    :: forall i hs o a. Fractional a
    => a
    -> Network i hs o a
    -> Network i hs o a
compensateDO d = \case
    NetOL w   -> NetOL w
    NetIL l n -> NetIL l (go n)
  where
    go  :: forall h hs'. ()
        => Network h hs' o a
        -> Network h hs' o a
    go = \case NetOL w   -> NetOL (compFLayer w)
               NetIL w n -> NetIL (compRLayer w) (go n)
    compFLayer
        :: forall i' o'. ()
        => FLayer i' o' a
        -> FLayer i' o' a
    compFLayer = over (tFLayerNodes . traverse . tNodeWeights) (d' *^)
    compRLayer
        :: forall i' o'. ()
        => RLayer i' o' a
        -> RLayer i' o' a
    compRLayer = over (tRLayerNodes . traverse . tRNodeWeights) $ \(wI, wS) ->
        let wI' = d' *^ wI
            wS' = d' *^ wS
        in  wI' `seq` wS' `seq` (wI', wS')
    d' = 1 / (1 - d)
{-# INLINE compensateDO #-}
