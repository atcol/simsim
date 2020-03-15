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

import           Options.Generic (ParseRecord, getRecord)
import           Prelude         (print)
import           RIO.Process
import           Simsim.Import
import           System.Random   (StdGen, getStdGen, newStdGen, setStdGen)

instance ParseRecord Options

-- | Run the simulation until it terminates
runActor :: (MonadIO m, Show a) => Actor m a -> (StdGen, ActorState a) -> m (Actor m a, StdGen, [ActorState a])
runActor = loop []
  where
    loop x r v = do
      (s, g', v') <- runReaderT r v
      let st = simStats $ astSim v'
      (SimulationStats n) <- takeMVar st
      putMVar st (SimulationStats (n + 1))
      liftIO $ print $ "New state: " ++ show v'
      case s of
        Terminate -> return (r, g', x ++ [v'])
        _         -> loop (x ++ [v']) r (g', v')

-- | Wrap @runActor@ with statistics gathering and logging
run :: (Show a) => [(Actor IO a, ActorState a)] -> RIO Simulation [(Actor IO a, [ActorState a])]
run pairs = do
  si@(Simulation _ _ opts stats) <- ask
  logInfo $ "Starting simulation " <> displayShow si
  g <-
    case seed opts >>= readMaybe of
      Just s -> liftIO $ setStdGen s >> getStdGen
      _      -> liftIO newStdGen
  logInfo $ "Seed is " <> displayShow g
  res <- liftIO $ mapConcurrently (doStep g) pairs
  simSt <- takeMVar stats
  logInfo $ "Simulation stats: " <> displayShow simSt
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
  ss <- newMVar $ SimulationStats 0
  withLogFunc lo $ \lf ->
    let sim = Simulation {simLogFunc = lf, simProcessContext = pc, simOptions = opts, simStats = ss}
     in runRIO sim (run $ map (\(a, v) -> (a, ActorSimState v sim)) x)
