{-# LANGUAGE RankNTypes #-}

module Simsim.Methods.MonteCarlo (
  -- | The monte carlo parameter type, with smart constructors e.g. 'uniformMonteCarlo'
  MonteCarlo(..)
  -- | The Monte Carlo simulation actor
  , monteCarlo
  , uniformMonteCarlo
  ) where

import           Data.Random                      (MonadRandom, RVar,
                                                   sampleState, uniform)
import           RIO                              (ask)
import           Simsim.Import                    (Actor, ActorState (..),
                                                   Status (..))

data MonteCarlo =
  MonteCarlo {
    -- | The problem space
    bounds    :: (Float, Float)
    -- | The predicate for testing if a random value is in the problem space
    , predicate :: Float -> Bool
    -- | The number of trials to run
    , trials    :: Int
    -- | The distribution, see e.g. 'uniformMonteCarlo'
    , dist      :: Float -> Float -> RVar Float
    -- | The interim results from each run
    , result   :: Maybe Float
    }

instance Show MonteCarlo where
  show (MonteCarlo b _ t _ r) =
    "MonteCarlo {bounds = " ++ show b ++ ", trials = " ++ show t ++ ", results = " ++ show r ++ "}"

-- | Create a 'MonteCarlo' using that samples from uniform distribution
uniformMonteCarlo :: (Float, Float) -> (Float -> Bool) -> Int -> Maybe MonteCarlo
uniformMonteCarlo _ _ 0 = Nothing
uniformMonteCarlo b@(l, u) p t
  | l < u     = Just $ MonteCarlo b p t uniform Nothing 
  | otherwise = Nothing

-- | Execute a step in a Monte Carlo simulation
monteCarlo :: MonadRandom m => Actor m MonteCarlo
monteCarlo = do
  (g, s@(ActorSimState mc@(MonteCarlo (a, b) p tri di _) _)) <- ask
  let (val, g') = sampleState (di a b) g
      tr = tri - 1
      newRes =
        if p val
          then Just val
          else Nothing
      status =
        if tr <= 0
          then Terminate
          else Continue
  return (status, g', s {astValue = (mc {result = newRes, trials = tr})})
