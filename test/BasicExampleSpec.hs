{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module BasicExampleSpec
  ( spec
  ) where

import           Data.List     (last, foldl)
import           Prelude       (print)
import           Simsim.Import
import           Simsim.Run
import           System.Random (randomR)
import           Test.Hspec

simCount :: Int
simCount = 11

startingTemp :: Double
startingTemp = 15.0

-- | A temperature sensor that increases/decreases by a random value in [-1, 1]
temperature :: Actor IO Double
temperature = do
  (g, s@(ActorSimState currentTemp sim)) <- ask -- | Get my current state and a StdGen as a source of randomness
  let (tempDelta, g') = randomR (-2.0, 2.0) g -- | Choose my temperature inc/dec in [-1, 1]
  (SimulationStats count) <- readMVar (simStats sim) -- | See how many runs there are
  liftIO $ print $ "Sim count is " ++ show count
  return $
    if count < simCount
      then (Continue, g', s {astValue = currentTemp + tempDelta})
      else (Terminate, g', s {astValue = currentTemp})

spec :: Spec
spec =
  describe "Basic Example" $
  it "Demonstrates a temperature sensor" $ do
    results <- runSim [(temperature, startingTemp)]
    let values = foldl (\a (_, x) -> a ++ map astValue x) [] results
    values `shouldNotBe` []
    liftIO $ print values
