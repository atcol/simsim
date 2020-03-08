{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where

import RIO
import RIO.Process
import System.Random

-- | Command line arguments
data Options = Options
  { optVerbose :: !Bool
  , optSeed :: !(Maybe Int)
  } deriving (Eq, Generic, Show)

data Simulation a = Simulation
  { simLogFunc :: !LogFunc
  , simProcessContext :: !ProcessContext
  , simOptions :: !Options
  , simName    :: !(Maybe Text) -- | The name of the sim to run, if registered programmatically
  , simWorld   :: !(MVar a)
  }

instance HasLogFunc (Simulation a) where
  logFuncL = lens simLogFunc (\x y -> x { simLogFunc = y })
instance HasProcessContext (Simulation a) where
  processContextL = lens simProcessContext (\x y -> x { simProcessContext = y })

-- | An indicator of state post @act@/invocation
data Status = Continue | Terminate
  deriving (Eq, Read, Show)

-- | An entity in the simulation that performs some activity/behaviour.
class (MonadIO m) => Actor m a where

  -- | Run the associated simulation for one "tick"/invocation
  act :: (RandomGen g) => ReaderT (g, a) m (Status, g, a)

