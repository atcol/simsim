{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Simsim.Run
  ( getValues
  , run
  , runSim
  ) where

import           Options.Generic (ParseRecord, getRecord)
import           Prelude         (print)
import           RIO.Process
import           Simsim.Import
import           System.Random   (StdGen, getStdGen, newStdGen, setStdGen)
import Data.List (foldl)

instance ParseRecord Options

-- | Run the simulation until it terminates
runActor :: (MonadIO m, Show a) => Actor m a -> (StdGen, ActorState a) -> m (Actor m a, StdGen, [ActorState a])
runActor = loop []
  where
    loop :: (MonadIO m, Show a) => [ActorState a] -> Actor m a -> (StdGen, ActorState a) -> m (Actor m a, StdGen, [ActorState a])
    loop x r v = do
      (s, g', v') <- runReaderT r v
      let st = simStats $ astSim v'
      (SimulationStats n) <- takeMVar st
      putMVar st (SimulationStats (n + 1))
--      logDebug $ "New state: " <> displayShow v'
      case s of
        Terminate -> return (r, g', x ++ [v'])
        _         -> loop (x ++ [v']) r (g', v')

-- | Wrap 'runActor' with statistics gathering and logging
run :: (Show a) => [(Actor IO a, ActorState a)] -> RIO Simulation [(Actor IO a, [ActorState a])]
run pairs = do
  si@(Simulation _ _ opts stats) <- ask
  logDebug $ "Starting simulation " <> displayShow si
  g <-
    case seed opts >>= readMaybe of
      Just s -> liftIO $ setStdGen s >> getStdGen
      _      -> liftIO newStdGen
  logDebug $ "Seed is " <> displayShow g
  res <- liftIO $ mapConcurrently (doStep g) pairs
  simSt <- takeMVar stats
  logDebug $ "Simulation stats: " <> displayShow simSt
  return res
  where
    doStep g (act, v) = do
      (_, _, vals) <- liftIO $ runActor act (g, v)
      return (act, vals)

-- | Entry-point for running a simulation from a set of @Actor@s and their initial state
runSim :: (Show a) => [(Actor IO a, a)] -> IO [(Actor IO a, [ActorState a])]
runSim x = do
  opts <- getRecord "Simsim :)" :: IO Options
  lo <- logOptionsHandle stderr (verbose opts)
  pc <- mkDefaultProcessContext
  ss <- newMVar $ SimulationStats 1
  withLogFunc lo $ \lf ->
    let sim = Simulation {simLogFunc = lf, simProcessContext = pc, simOptions = opts, simStats = ss}
     in runRIO sim (run $ map (\(a, v) -> (a, ActorSimState v sim)) x)

-- | Extract the value from the result of a simulation, e.g. 'runSim'
getValues :: [(actor, [ActorState a])] -> [a]
getValues = foldl (\a (_, x) -> a ++ map astValue x) []
