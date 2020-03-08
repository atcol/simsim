{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Run
  ( run
  ) where

import           Import
import           Prelude       (print)
import           System.Random (RandomGen, getStdGen, mkStdGen, newStdGen,
                                random, setStdGen)
import Text.Printf (printf)

data Vehicle
  = Car
      { carName :: Text
      }
  | Motorcycle
      { mcName :: Text
      }
  deriving (Eq, Read, Show)

instance Actor IO Vehicle where
  act = do
    (g, a) <- ask
    let (n :: Int, g') = random g
    case a of
      (Motorcycle name) -> do
        liftIO $ print $ "I'm terminating " ++ show name
        return (Terminate, g, a)
      (Car name) -> if even n then
                      do liftIO $ print $ "Car terminating: " ++ show name
                         return (Terminate, g', a)
                      else
                      do liftIO $ print $ "Car continuing " ++ show name
                         return (Continue, g', a)

experiment :: (Actor IO a, RandomGen g) => Text -> Int -> g -> a -> IO (g, a)
experiment t n g a
  | n > 0 = do
    (s, g', a') <- runReaderT act (g, a)
    case s of
      Terminate -> return (g', a')
      _         -> experiment t (n - 1) g' a'
  | otherwise = return (g, a)

run :: RIO (Simulation a) ()
run = do
  (Simulation _ pc opts world) <- ask
  let seed = optSeed opts
  g <-
    case seed of
      Just s -> liftIO $ setStdGen (mkStdGen s) >> getStdGen
      _      -> liftIO newStdGen
  logInfo $ "Seed is " <> displayShow seed
  let acts = [Car "Sensible", Motorcycle "Crazy"]
  res <- liftIO $ mapConcurrently (experiment "Vehicle" 10 g) acts
  logInfo $ "Simulation complete: " <> displayShow res
  
 runSim = do
  opts <- getRecord "Simsim :)" :: IO Options
  lo <- logOptionsHandle stderr (optVerbose opts)
  pc <- mkDefaultProcessContext
  w <- newMVar "Hi!"
  withLogFunc lo $ \lf ->
    let sim = Simulation {simLogFunc = lf, simProcessContext = pc, simOptions = opts, simWorld = w}
     in runRIO sim run
