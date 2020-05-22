{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Simsim.Methods.MonteCarlo
  ( MonteCarlo(..) -- ^ The monte carlo parameter type, with smart constructors e.g. 'uniformMonteCarlo'
  , MonteCarloResult(..) -- ^ The result of a Monte Carlo simulation step
  -- | The Monte Carlo simulation actor
  , monteCarlo
  , uniformMonteCarlo
  ) where

import           Control.Monad.IO.Class           (MonadIO)
import           Data.Maybe                       (fromMaybe)
import           Data.Random                      (Distribution, sampleState)
import           Data.Random.Distribution         (rvar)
import           Data.Random.Distribution.Uniform (Uniform (..))
import           RIO                              (ask)
import           Simsim.Import                    (Actor, ActorState (..),
                                                   Status (..))

data MonteCarlo a =
  MonteCarlo
    { mcTrials :: Int -- ^ The number of trials to run
    , mcResult :: Maybe (MonteCarloResult a) -- ^ The result wrapper of the step
    }
  deriving (Eq, Show)

-- | The output of a step in a Monte Carlo simulation
data MonteCarloResult a =
  MonteCarloResult
    { mcrCount :: Int -- ^ The number of invocations
    , mcrHits  :: Int -- ^ The number of hits
    , mcrMiss  :: Int -- ^ The number of misses
    , mcrTotal :: a -- ^ The total at the end of this step
    , mcrValue :: a -- ^ The result from a step
    }
  deriving (Eq, Show)

-- | The predicate for a monte carlo, evaluated against a sample from its problem space
type MonteCarloPredicate a = (a -> Bool)

-- | Create a 'MonteCarlo' that samples @x@ from the uniform distribution
uniformMonteCarlo ::
     (MonadIO m, Num t, Ord t, Distribution Uniform t)
  => (t, t)
  -> MonteCarloPredicate t
  -> Int
  -> Maybe (Actor m (MonteCarlo t), MonteCarlo n)
uniformMonteCarlo _ _ 0 = Nothing
uniformMonteCarlo b@(l, u) p t
  | l < u = Just (monteCarlo p (Uniform l u), MonteCarlo t Nothing)
  | otherwise = Nothing

-- | Execute a step in a Monte Carlo simulation
monteCarlo :: (Num t, MonadIO m, Distribution d t) => MonteCarloPredicate t -> d t -> Actor m (MonteCarlo t)
monteCarlo p d = do
  (g, s@(ActorSimState mc@(MonteCarlo tri res) _)) <- ask
  let (val, g') = sampleState (rvar d) g
      tr = tri - 1
      Just r =
        Just $ fromMaybe MonteCarloResult {mcrCount = 0, mcrTotal = 0, mcrHits = 0, mcrMiss = 0, mcrValue = val} res
      newRes =
        Just $
        if p val
          then r {mcrCount = mcrCount r + 1, mcrTotal = mcrTotal r + val, mcrHits = mcrHits r + 1, mcrValue = val}
          else r {mcrCount = mcrCount r + 1, mcrMiss = mcrMiss r + 1, mcrValue = val}
      status =
        if tr <= 0
          then Terminate
          else Continue
  return (status, g', s {astValue = (mc {mcResult = newRes, mcTrials = tr})})
