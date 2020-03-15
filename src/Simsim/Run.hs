{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Simsim.Run
  ( run
  , runSim
  ) where

import           Simsim.Import
import           Options.Generic (ParseRecord, getRecord)
import           Prelude         (print)
import           RIO.Process
import           System.Random   (StdGen, getStdGen, newStdGen, setStdGen)

data Vehicle
  = Car
      { carName :: Text
      }
  | Motorcycle
      { mcName :: Text
      }
  deriving (Eq, Read, Show)

instance ParseRecord Options

-- | Run the simulation until it terminates
runActor :: (MonadIO m, Show a) => Actor m a -> (StdGen, ActorState a) -> m (StdGen, ActorState a)
runActor r v = do
  (s, g', v') <- runReaderT r v
  let st = simStats $ astSim v'
  (SimulationStats n) <- takeMVar st
  putMVar st (SimulationStats (n + 1))
  liftIO $ print $ "New state: " ++ show v'
  case s of
    Terminate -> return (g', v')
    _         -> runActor r (g', v')

-- | Wrap @runActor@ with statistics gathering and logging
run :: (Show a) => [(Actor IO a, ActorState a)] -> RIO Simulation [ActorState a]
run pairs = do
  si@(Simulation _ _ opts stats) <- ask
  logInfo $ "Starting simulation " <> displayShow si
  g <-
    case seed opts >>= readMaybe of
      Just s -> liftIO $ setStdGen s >> getStdGen
      _ -> liftIO newStdGen
  logInfo $ "Seed is " <> displayShow g
  res <- liftIO $ mapConcurrently (doStep g) pairs
  logInfo $ "Simulation complete: " <> displayShow res
  simSt <- takeMVar stats
  logInfo $ "Simulation stats: " <> displayShow simSt
  return $ map snd res
  where
    doStep g (act, v) = liftIO $ runActor act (g, v)

-- | Entrypoint for running a simulation from a set of @Actor@s and their initial state
runSim :: (Show a) => [(Actor IO a, a)] -> IO [ActorState a] 
runSim x = do
  opts <- getRecord "Simsim :)" :: IO Options
  lo <- logOptionsHandle stderr (verbose opts)
  pc <- mkDefaultProcessContext
  ss <- newMVar $ SimulationStats 0
  withLogFunc lo $ \lf ->
    let sim = Simulation {simLogFunc = lf, simProcessContext = pc, simOptions = opts, simStats = ss}
     in runRIO sim (run $ map (\(a, v) -> (a, ActorSimState v sim)) x)
