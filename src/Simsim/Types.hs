{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE Rank2Types            #-}

module Simsim.Types
  ( Actor
  , ActorState(..)
  , Options(..)
  , Simulation(..)
  , SimulationStats(..)
  , Status(..)
  ) where

import           RIO
import           RIO.Process
import           System.Random

-- | Command line arguments
data Options =
  Options
    { verbose :: !Bool
    , seed    :: !(Maybe String)
    }
  deriving (Generic, Show)

-- | Counters for execution
newtype SimulationStats =
  SimulationStats { ssCount :: Int }
  deriving (Show)

-- | The overall type containing real time definition of the simulation currently executing
data Simulation =
  Simulation
    { simLogFunc        :: !LogFunc
    , simProcessContext :: !ProcessContext
    , simOptions        :: !Options
    , simStats          :: !(MVar SimulationStats)
    }

data ActorState a =
  ActorSimState
    { astValue :: a
    , astSim   :: Simulation
    }

instance (Show a) => Show (ActorState a) where
  show (ActorSimState a s) = "ActorState (" ++ show a ++ ", " ++ show s ++ ")"

instance Show Simulation where
  show (Simulation _ _ opts _) = "Simulation _ _ " ++ show opts ++ ")"

-- | An entity in the simulation that performs some activity/behaviour.
type Actor m a = ReaderT (StdGen, ActorState a) m (Status, StdGen, ActorState a)

instance HasLogFunc Simulation where
  logFuncL = lens simLogFunc (\x y -> x {simLogFunc = y})

instance HasProcessContext Simulation where
  processContextL = lens simProcessContext (\x y -> x {simProcessContext = y})

-- | An indicator of state post @act@/invocation
data Status
  = Continue
  | Terminate
  deriving (Eq, Read, Show)
