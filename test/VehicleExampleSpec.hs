{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module VehicleExampleSpec
  ( spec
  ) where

import           RIO.List      (foldl)
import           Simsim.Import
import           Simsim.Run
import           Test.Hspec

data Mode
  = Moving
  | Parking
  | Parked
  deriving (Eq, Show)

data Vehicle =
  Car Text Double Mode
  deriving (Eq, Show)

maxSpeed :: Double
maxSpeed = 30.0

vehicleActor :: Actor IO Vehicle
vehicleActor = do
  (g, s@(ActorSimState (Car n speed m) sim)) <- ask
  (SimulationStats count) <- readMVar (simStats sim)
  case m of
    Parked -> return (Continue, g, s {astValue = Car n 5 Moving})
    Parking ->
      if (speed - 2) <= 5
        then return (Terminate, g, s {astValue = Car n 0 Parked})
        else return (Continue, g, s {astValue = Car n 3 Parking})
    Moving ->
      let startParking = count >= 10 || speed == maxSpeed
          newSpeed
            | startParking = 10
            | speed < maxSpeed = speed + ((maxSpeed - speed) / 2)
            | otherwise = maxSpeed
          mode =
            if startParking
              then Parking
              else Moving
       in return (Continue, g, s {astValue = Car n newSpeed mode})

spec :: Spec
spec =
  describe "Vehicle Example" $ it "Demonstrates a simple Vehicle simulation" $ do
    let c1 = Car "Volvo" 50 Parked
    results <- runSim [(vehicleActor, c1)]
    let values = foldl (\a (_, x) -> a ++ map astValue x) [] results
    values `shouldNotBe` []
    values `shouldSatisfy` (==) 0 .
      foldl
        (\n (Car _ speed _) ->
           if speed > maxSpeed
             then n + 1
             else n)
        (0 :: Int)
