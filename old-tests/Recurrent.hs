{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

import Data.Neural.Recurrent
import Control.Applicative
import Data.Neural.Recurrent.Train
import Data.Neural.Types
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Control.Monad
import Control.Monad.Random
import Data.Char
import qualified Data.Vector as V
import Linear.V
import Data.Ord
import Linear
import Data.Neural.Utility
import Data.Maybe
import Data.Foldable

genIx :: (Dim n, Num a) => Int -> Int -> Maybe (V n a)
genIx n i = fromVector . V.generate n $ \i' -> if i' == i then 1 else 0

ixMax :: (Foldable t, Ord a) => t a -> Int
ixMax = fst . maximumBy (comparing snd) . zip [0..] . toList

source :: [V 5 Double]
-- source = mapMaybe (genIx 5) $ cycle [0,1,2,3,4,3,2,1]
source = mapMaybe (genIx 5) $ cycle [0,1,2,3,4,0,4,1,3,2]

inps :: [V 5 Double]
inps = take 20 source

outs :: [V 5 Double]
outs = tail source

ios :: [(V 5 Double, V 5 Double)]
ios = zip inps outs

inp0 :: V 5 Double
Just inp0 = genIx 5 0

main :: IO ()
main = do
    g <- newStdGen
    flip evalRandT g $ do

      net0 <- randomNetwork :: RandT StdGen IO (Network 5 '[10] 5 Double)

      flip evalStateT net0 . forM_ (iterate tail (zip source (tail source))) $ \ios0 -> do
        -- R.trainSeries naLogLog (R.nudgeNetworkN 0.1) 1 0.5 ios 5000
        -- replicateM_ 500 . modify $ trainSeries naLogLog 0.01 0.01 ios
        let ios = take 20 ios0
        replicateM_ 250 . modify $ trainSeries naLogLog 0.01 0.01 ios

        totErr2 <- gets (\n -> fst $ seriesError naLogLog n ios)
        liftIO . print $ totErr2 / fromIntegral (length ios)

        feedBack <- gets (\n -> map ixMax . take 100 $ runNetFeedback_ naLogLog (fromJust . genIx 5 . ixMax) n inp0)
        parTest <- gets (\n -> map ixMax . runNetStream_ naLogLog n . take 100 $ source)
        netStates <- gets (getConst . tNetStates (\s -> Const [toVector s]))
        -- feedBack <- gets (\n -> map ixMax . take 1000 $ runNetFeedback_ naLogLog id n inp0)
        liftIO $ do
          -- print netStates
          putStrLn $ map (intToDigit . ixMax) . take 100 . tail $ source
          putStrLn $ map intToDigit parTest
          putStrLn $ map intToDigit feedBack
