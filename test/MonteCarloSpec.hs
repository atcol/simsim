{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module MonteCarloSpec
  ( spec
  ) where

import           Data.List                 (foldl)
import           Data.Maybe                (fromJust)
import           Prelude                   (head)
import           Simsim.Import             hiding (assert)
import           Simsim.Methods.MonteCarlo
import           Simsim.Run
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic   (assert, monadicIO)

spec :: Spec
spec =
  describe "MonteCarlo" $
  context "QuickCheck - Successfully runs a simulation" $ do
    it "produces values within bounds" $ property prop_withinBounds
    it "always runs specified # of trials" $ property prop_runsTrials
    it "evaluates using the predicate" $ property prop_predicate

prop_withinBounds :: (NonNegative Float, NonNegative Float) -> Property
prop_withinBounds (NonNegative a, NonNegative b) =
  monadicIO $
  case uniformMonteCarlo (a, b) (const True) 1 of
    Nothing -> assert $ b == 0 || a >= b
    Just params -> do
      outputs <- liftIO $ runSim [(monteCarlo, params)]
      let values = foldl (\a' (_, x) -> a' ++ map astValue x) [] outputs
          firstRes = head values
          res = result firstRes
      -- Assert the interval is in [i, j)
      liftIO $ isJust res `shouldBe` True
      liftIO $ fromJust res `shouldSatisfy` (<=) a
      liftIO $ fromJust res `shouldSatisfy` (>) b

prop_runsTrials :: NonNegative Int -> Property
prop_runsTrials (NonNegative numTrials) =
  monadicIO $
  case uniformMonteCarlo (0, 1) (const True) numTrials of
    Nothing -> liftIO $ numTrials `shouldSatisfy` (>=) 0
    Just params -> do
      outputs <- liftIO $ runSim [(monteCarlo, params)]
      let values = foldl (\a (_, x) -> a ++ map (result . astValue) x) [] outputs
          generated = filter (\n -> n >= 0 && n < 1) $ map fromJust values
      liftIO $ length generated `shouldBe` numTrials

prop_predicate :: NonNegative Int -> Property
prop_predicate (NonNegative numTrials) =
  monadicIO $
  case uniformMonteCarlo (0, 1) (const False) numTrials of
    Nothing -> assert $ numTrials <= 0
    Just params -> do
      outputs <- liftIO $ runSim [(monteCarlo, params)]
      let values = foldl (\a (_, x) -> a ++ map (result . astValue) x) [] outputs
          found = catMaybes values
      liftIO $ length values `shouldBe` numTrials
      liftIO $ length found `shouldBe` 0
