{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}


import Control.Monad.Random as R
import Control.Monad.State.Strict
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Control.Arrow
import Data.Neural.Recurrent
import Data.Neural.Recurrent.Train
import Data.Neural.Utility
import Data.Ord
import Data.Proxy
import Data.Reflection
import Data.Tuple
import GHC.TypeLits
import Linear.V
import qualified Data.Binary          as B
import qualified Data.ByteString.Lazy as B
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Vector          as V

genIx :: (KnownNat n, Num a) => Int -> Int -> Maybe (V n a)
genIx n i = fromVector . V.generate n $ \i' -> if i' == i then 1 else 0

ixMax :: (Foldable t, Ord a) => t a -> Int
ixMax = fst . maximumBy (comparing snd) . zip [0..] . toList

ixSort :: (Foldable t, Ord a) => t a -> [Int]
ixSort = map fst . sortBy (flip (comparing snd)) . zip [0..] . toList

sanitize :: Char -> Char
sanitize c | isPrint c = c
           | otherwise = '#'

main :: IO ()
main = do
    g <- newStdGen
    holmes <- readFile "holmes.txt"
    let charMap = M.fromList
                . (`zip` [0..])
                . S.toList
                . S.fromList
                $ holmes
        charSpace = length charMap
        charMap' = M.fromList . map swap . M.toList $ charMap
        toChar :: Foldable t => t Double -> Char
        toChar = (charMap' M.!) . ixMax
        randNext :: (MonadRandom m, Foldable t) => t Double -> m Char
        randNext v = do
            let weights = zip (M.keys charMap) (realToFrac . max 0 <$> toList v)
            R.fromList weights
    print $ M.keys charMap
    print $ length (M.keys charMap)
    print $ length holmes

    reifyNat (fromIntegral (length charMap)) $ \p@(Proxy :: Proxy n) -> do
      let holmesV :: [V n Double]
          holmesV = mapMaybe (genIx charSpace . (charMap M.!)) holmes
          allIos  = zip holmesV (tail holmesV)
          windows = transpose . take 50 $ iterate tail allIos
          i0 = head holmesV

      (trained,_) <- flip evalRandT g $ do
        savedNet <- liftIO $ B.decodeFileOrFail "holmes.net"
        net0 <- case savedNet of
          Left _  -> do
            liftIO $ print "Generating new initial net."
            randomNetwork'
          Right n -> do
            liftIO $ putStrLn "Loaded old net!"
            return (n :: Network n '[75] n Double)
        -- net0 <- randomNetwork' :: RandT StdGen IO (Network n '[75] n Double)

        flip execStateT (net0, 0 :: Int) . replicateM_ 2 . forM_ windows $ \ios -> do
          -- trainSeries naLogLog (nudgeNetworkN 0.5) 1 0.1 ios 20
          -- modify $ \n -> trainSeriesGD naLogLog 0.05 0.5 ios n 1
          -- replicateM_ 2 $ modify (adjustNetworkGD naLogLog 0.05 0.25 ios)
          -- replicateM_ 20 . modify $ trainSeries naLogLog 0.10 0.10 ios
          replicateM_ 1 . modify . first $ trainSeries naLogLog 0.50 0.05 ios

          stp <- gets snd
          when (stp `mod` 10 == 0) $ do
            totErr2 <- gets (\(n,_) -> fst $ seriesError naLogLog n ios)
            liftIO . print $ totErr2 / fromIntegral (length ios)

            parTest <- gets (\(n,_) -> runNetStream_ naLogLog n $ map fst ios)

            liftIO $ do
              forM_ (zip parTest (tail ios)) $ \(v, (v', _)) -> do
                let topPicks = (charMap' M.!) <$> ixSort v
                    goal = toChar v'
                    Just rankOfPick = goal `elemIndex` topPicks
                putStrLn $ sanitize goal : ":\t|" ++ map sanitize (take 30 topPicks) ++ "|\t" ++ show (rankOfPick + 1)


              putStrLn $ map (sanitize . toChar . fst) ios
              putStrLn . map (sanitize . toChar) $ fst (head ios) : parTest

            feedBack <- do
              n <- gets fst
              runNetFeedbackM_ naLogLog (fmap (fromJust . genIx charSpace . (charMap M.!)) . randNext) n 100 (fst (head ios))

            liftIO . putStrLn . map (sanitize . toChar) $ fst (head ios) : feedBack


            -- feedBack <- gets (\n -> map (sanitize . toChar) . (fst (head ios) :) . take 100 . runNetFeedback_ naLogLog (fromJust . genIx charSpace . ixMax) n . fst $ head ios)
            -- liftIO $ putStrLn feedBack

            liftIO . B.encodeFile "holmes.net" =<< gets fst
          modify . second $ (+1)


      let totErr2 = fst $ seriesError naLogLog trained allIos
          totFeedback = map toChar $ runNetFeedback_ naLogLog (fromJust . genIx charSpace . ixMax) trained i0

      putStrLn $ take 10000 totFeedback
