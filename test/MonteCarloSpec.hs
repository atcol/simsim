{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module MonteCarloSpec
  ( spec
  ) where

import           Data.List                 (foldl, last)
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
  describe "MonteCarlo" $ do
    context "Examples" $
      it "Flip a coin" $ do
        let pred = (<= 0.5)
            ab@(a, b) = (0 :: Float, 1)
            mc = uniformMonteCarlo ab pred 10000
            p = fromJust mc
        r <- liftIO $ runSim [p]
        let values = foldl (\a' (_, x) -> a' ++ map astValue x) [] r
            lastVal = last values
            res = mcResult lastVal
            allResults = map mcrValue $ catMaybes $ map mcResult values
            (MonteCarloResult count hits miss tot mValue) = fromJust res
            estimate = toRational hits / toRational count
        liftIO $ count `shouldBe` 10000
        liftIO $ hits + miss `shouldBe` count
        liftIO $ tot `shouldBe` sum (filter pred allResults)
        liftIO $ mValue `shouldSatisfy` (<=) a
        liftIO $ mValue `shouldSatisfy` (>) b
        estimate `shouldNotSatisfy` (< 0.49)
    context "QuickCheck - Successfully runs a simulation" $ do
      it "produces values within bounds" $ property prop_withinBounds
      it "always runs specified # of trials" $ property prop_runsTrials
      it "evaluates using the predicate" $ property prop_predicate

--      it "Buffon and Laplace estimate Pi" $ do
--        let pred = (<= 0.5)
--            mc = uniformMonteCarlo (0, 1) pred 10000
--            params@(MonteCarlo (a, b) _ _ _ _) = fromJust mc
--        r <- liftIO $ runSim [(monteCarlo, params)]
--        let values = foldl (\a' (_, x) -> a' ++ map astValue x) [] r
--            lastVal = last values
--            res = mcResult lastVal
--            allResults = map mcrValue $ catMaybes $ map mcResult values
--            (MonteCarloResult count hits miss tot mValue) = fromJust res
--            estimate = toRational hits / toRational count
--        liftIO $ count `shouldBe` 10000
--        liftIO $ hits + miss `shouldBe` count
--        liftIO $ tot `shouldBe` sum (filter pred allResults)
--        liftIO $ mValue `shouldSatisfy` (<=) a
--        liftIO $ mValue `shouldSatisfy` (>) b
--        estimate `shouldNotSatisfy` (< 0.49)

prop_withinBounds :: (NonNegative Float, NonNegative Float) -> Property
prop_withinBounds (NonNegative a, NonNegative b) =
  monadicIO $
  case uniformMonteCarlo (a, b) (const True) 1 of
    Nothing -> assert $ b == 0 || a >= b
    Just params -> do
      outputs <- liftIO $ runSim [params]
      let values = getValues outputs
          firstRes = head values
          res = mcResult firstRes
          (MonteCarloResult count hits miss tot value) = fromJust res
      -- Assert the interval is in [i, j)
      liftIO $ count `shouldBe` 1
      liftIO $ hits `shouldBe` 1
      liftIO $ miss `shouldBe` 0
      liftIO $ tot `shouldBe` value
      liftIO $ value `shouldSatisfy` (<=) a
      liftIO $ value `shouldSatisfy` (>) b

prop_runsTrials :: NonNegative Int -> Property
prop_runsTrials (NonNegative numTrials) =
  monadicIO $
  case uniformMonteCarlo (0 :: Float, 1) (const True) numTrials of
    Nothing -> liftIO $ numTrials `shouldSatisfy` (>=) 0
    Just pair -> do
      outputs <- liftIO $ runSim [pair]
      let values = map mcResult $ getValues outputs
          generated = filter (\n -> n >= 0 && n < 1) $ map (mcrValue . fromJust) values
          allResults = catMaybes values
          (MonteCarloResult count hits miss tot mValue) = last allResults
      liftIO $ length generated `shouldBe` numTrials
      liftIO $ count `shouldBe` numTrials
      liftIO $ hits + miss `shouldBe` count
      liftIO $ tot `shouldBe` sum (map mcrValue allResults)
      liftIO $ mValue `shouldSatisfy` (> 0)

prop_predicate :: NonNegative Int -> Property
prop_predicate (NonNegative numTrials) =
  monadicIO $
  case uniformMonteCarlo (0 :: Float, 1) (const False) numTrials of
    Nothing -> assert $ numTrials <= 0
    Just params -> do
      outputs <- liftIO $ runSim [params]
      let values = map mcResult $ getValues outputs
          found = catMaybes values
      liftIO $ length values `shouldBe` numTrials
      liftIO $ length found `shouldBe` numTrials
