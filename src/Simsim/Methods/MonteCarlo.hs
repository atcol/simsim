{-# LANGUAGE RankNTypes #-}

module Simsim.Methods.MonteCarlo
  -- | The monte carlo parameter type, with smart constructors e.g. 'uniformMonteCarlo'
  ( MonteCarlo(..)
  , MonteCarloResult(..)
  -- | The Monte Carlo simulation actor
  , monteCarlo
  , uniformMonteCarlo
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe             (fromMaybe)
import           Data.Random            (MonadRandom, RVar, sampleState,
                                         uniform)
import           RIO                    (ask)
import           Simsim.Import          (Actor, ActorState (..), Status (..))

data MonteCarlo =
  MonteCarlo
    { mcBounds :: (Float, Float) -- ^ The problem space
    , mcPred   :: Float -> Bool -- ^ The predicate for testing if a random value is in the problem space
    , mcTrials :: Int -- ^ The number of trials to run
    , mcDist   :: Float -> Float -> RVar Float -- ^ The distribution, see e.g. 'uniformMonteCarlo'
    , mcResult :: Maybe MonteCarloResult
    }

data MonteCarloResult =
  MonteCarloResult
    { mcrCount :: Int -- ^ The number of invocations
    , mcrHits  :: Int -- ^ The number of hits
    , mcrMiss  :: Int -- ^ The number of misses
    , mcrTotal :: Float -- ^ The total at the end of this step
    , mcrValue :: Float -- ^ The result from a step
    }
  deriving (Eq, Show)

instance Show MonteCarlo where
  show (MonteCarlo b _ t _ r) =
    "MonteCarlo {bounds = " ++ show b ++ ", trials = " ++ show t ++ ", results = " ++ show r ++ "}"

-- | Create a 'MonteCarlo' using that samples from a uniform distribution
uniformMonteCarlo :: (Float, Float) -> (Float -> Bool) -> Int -> Maybe MonteCarlo
uniformMonteCarlo _ _ 0 = Nothing
uniformMonteCarlo b@(l, u) p t
  | l < u = Just $ MonteCarlo b p t uniform Nothing
  | otherwise = Nothing

-- | Execute a step in a Monte Carlo simulation
monteCarlo :: (MonadIO m, MonadRandom m) => Actor m MonteCarlo
monteCarlo = do
  (g, s@(ActorSimState mc@(MonteCarlo (a, b) p tri di res) _)) <- ask
  let (val, g') = sampleState (di a b) g
      tr = tri - 1
      Just r =
        Just $
        fromMaybe MonteCarloResult {mcrCount = 0, mcrTotal = 0, mcrHits = 0, mcrMiss = 0, mcrValue = val} res
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
