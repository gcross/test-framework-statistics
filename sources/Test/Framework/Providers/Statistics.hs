-- @+leo-ver=4-thin
-- @+node:gcross.20100107191635.1412:@thin sources/Test/Framework/Providers/Statistics.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100107114651.1477:<< Language extensions >>
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
-- @-node:gcross.20100107114651.1477:<< Language extensions >>
-- @nl

module Test.Framework.Providers.Statistics
        (testBernoulli
        ,testDistribution
        ,testWalkDistribution
        ,computeTestCountForThresholds
        ) where

-- @<< Import needed modules >>
-- @+node:gcross.20100107114651.1460:<< Import needed modules >>
import Control.Arrow

import Data.List

import Test.Framework.Providers.API

import Text.Printf

import Statistics.Distribution
import Statistics.Distribution.Normal

import Debug.Trace
import System.IO.Unsafe
-- @nonl
-- @-node:gcross.20100107114651.1460:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100107114651.1461:Types
-- @+node:gcross.20100107114651.1462:TestRunning
newtype TestRunning = TestRunning (Int,Int)

instance Show TestRunning where
    show (TestRunning (current,final)) = show current ++ "/" ++ show final
-- @-node:gcross.20100107114651.1462:TestRunning
-- @+node:gcross.20100107114651.1463:TestStatus
data TestStatus =
    TestOK
  | TestFailure String
-- @-node:gcross.20100107114651.1463:TestStatus
-- @+node:gcross.20100107114651.1464:TestResult
data TestResult = TestResult
    {   testStatus :: TestStatus
    }

instance Show TestResult where
    show test_result =
        case testStatus test_result of
            TestOK -> "OK"
            TestFailure message -> "Failed: " ++ message

instance TestResultlike TestRunning TestResult where
    testSucceeded test_result =
        case testStatus test_result of
            TestOK -> True
            _ -> False
-- @-node:gcross.20100107114651.1464:TestResult
-- @+node:gcross.20100107114651.1465:TestCase
data TestCase datum accumulator =
    TestCase
    {   testCount          :: Int
    ,   testDataGenerator  :: TestDataGenerator datum
    ,   testDataSeed       :: accumulator
    ,   testDataProcessor  :: datum -> accumulator -> accumulator
    ,   testDataSummarizer :: accumulator -> TestResult
    }

instance Testlike TestRunning TestResult (TestCase datum accumulator) where
    testTypeName _ = "Statistical Tests"
    runTest options test_case = runImprovingIO $ go number_of_points (testDataGenerator test_case) (testDataSeed test_case)
      where
        number_of_points = testCount test_case
        go :: Int -> TestDataGenerator datum -> accumulator -> ImprovingIO TestRunning TestResult TestResult
        go 0 _ !accum = return $ testDataSummarizer test_case accum
        go n (!TestDataGenerator generator) !accum = do
            yieldImprovement (TestRunning (number_of_points-n,number_of_points))
            (datum, next_generator) <- liftIO generator
            go (n-1) next_generator $ testDataProcessor test_case datum accum
-- @-node:gcross.20100107114651.1465:TestCase
-- @+node:gcross.20100109140101.1520:TestDataGenerator
newtype TestDataGenerator datum = TestDataGenerator { unwrapTestDataGenerator :: IO (datum, TestDataGenerator datum) }
-- @-node:gcross.20100109140101.1520:TestDataGenerator
-- @-node:gcross.20100107114651.1461:Types
-- @+node:gcross.20100107114651.1466:Functions
-- @+node:gcross.20100107114651.1467:computeChebychevBound
computeChebychevBound :: Int -> Double -> Double -> Double -> Double
computeChebychevBound count mean variance observed_sum =
    let count_as_double = fromIntegral count
        observed_mean = observed_sum / count_as_double
        delta = observed_mean - mean
    in variance / (count_as_double * delta * delta)
-- @-node:gcross.20100107114651.1467:computeChebychevBound
-- @+node:gcross.20100107191635.1612:computeTestCountForThresholds
computeTestCountForThresholds :: Double -> Double -> Int
computeTestCountForThresholds mean_threshold probability_threshold =
    let current_threshold = -quantile standard probability_threshold
    in if current_threshold < mean_threshold
        then 1
        else
            let sqrt_multipler = (ceiling $ current_threshold / mean_threshold)
            in sqrt_multipler * sqrt_multipler

-- @-node:gcross.20100107191635.1612:computeTestCountForThresholds
-- @+node:gcross.20100107191635.1861:computeKolmogorovProbability
computeKolmogorovProbability :: Double -> Double
computeKolmogorovProbability z
   | u < 0.2
      = 1
   | u < 0.755
      = 1 - w * (exp(c1/v)+exp(c2/v)+exp(c3/v))/u
   | u < 6.8116
      = 2 * sum [ sign * exp(coef*v)
                | (sign,coef) <- take (1 `max` round (3/u)) coefs
                ]
   | otherwise
      = 0
  where
    u = abs z
    v = u*u
    w = 2.50662827
    c1 = -pi**2/8
    c2 = 9*c1
    c3 = 25*c1
    coefs = [(1,-2),(-1,-8),(1,-18),(-1,-32)]
-- @-node:gcross.20100107191635.1861:computeKolmogorovProbability
-- @+node:gcross.20100109130159.1282:computeKolmogorovDistanceFromExact
computeKolmogorovDistanceFromExact :: (Double -> Double) -> [Double] -> Double
computeKolmogorovDistanceFromExact computeExactCumulative samples =
    go increment_fraction (sort samples) 0
  where
    increment_fraction = 1 / fromIntegral (length samples)
    go _ [] !maximum_distance = maximum_distance
    go !current_fraction (sample:rest_samples) !maximum_distance =
        go (current_fraction+increment_fraction)
           rest_samples
           (maximum_distance `max` abs (current_fraction - computeExactCumulative sample))
-- @-node:gcross.20100109130159.1282:computeKolmogorovDistanceFromExact
-- @+node:gcross.20100109130159.1284:computeKolmogorovStatisticFromExact
computeKolmogorovStatisticFromExact :: (Double -> Double) -> [Double] -> Double
computeKolmogorovStatisticFromExact computeExactCumulative samples =
    computeKolmogorovProbability (sqrt_number_of_samples*distance)
  where
    sqrt_number_of_samples = sqrt . fromIntegral . length $ samples
    distance = computeKolmogorovDistanceFromExact computeExactCumulative samples
-- @-node:gcross.20100109130159.1284:computeKolmogorovStatisticFromExact
-- @+node:gcross.20100109140101.1521:independentGenerator
independentGenerator :: IO datum -> TestDataGenerator datum
independentGenerator generator = wrapped_generator
  where wrapped_generator = TestDataGenerator $ fmap (\x -> (x,wrapped_generator)) generator
-- @-node:gcross.20100109140101.1521:independentGenerator
-- @-node:gcross.20100107114651.1466:Functions
-- @+node:gcross.20100107114651.1474:Interface
-- @+node:gcross.20100107114651.1469:testBernoulli
testBernoulli :: String -> Double -> Double -> Double -> IO Bool -> Test
testBernoulli name probability_of_True mean_threshold minimum_probability_threshold generator =
    Test name $
        TestCase
        {   testCount          = number_of_tests
        ,   testDataGenerator  = independentGenerator generator
        ,   testDataSeed       = 0 :: Int
        ,   testDataProcessor  = \result -> if result then (+1) else id
        ,   testDataSummarizer = \count ->
                let mean_ = (fromIntegral number_of_tests * probability_of_True)
                    variance_ = mean_ * (1-probability_of_True)
                    distribution = fromParams mean_ variance_
                    cumulative_probability = (cumulative distribution . fromIntegral $ count)
                    total_probability = 2 *
                        if cumulative_probability < 0.5
                            then cumulative_probability
                            else 1-cumulative_probability
                in TestResult $
                    if total_probability < minimum_probability_threshold
                    then TestFailure $
                            printf "Observed count was %i/%i.  Computed probablility of being this far from the mean was %f < %f."
                                count
                                number_of_tests
                                total_probability
                                minimum_probability_threshold
                    else TestOK
        }
  where
    number_of_tests = computeTestCountForThresholds mean_threshold minimum_probability_threshold
-- @-node:gcross.20100107114651.1469:testBernoulli
-- @+node:gcross.20100109140101.1519:testDistribution
testDistribution :: String -> (Double -> Double) -> Int -> Double -> IO Double -> Test
testDistribution name computeExactCumulative number_of_tests minimum_probability_threshold generator =
    Test name $
        TestCase
        {   testCount          = number_of_tests
        ,   testDataGenerator  = independentGenerator generator
        ,   testDataSeed       = [] :: [Double]
        ,   testDataProcessor  = \(!sample) (!previous_samples) -> sample:previous_samples
        ,   testDataSummarizer = \samples -> TestResult $
                let statistic = computeKolmogorovStatisticFromExact computeExactCumulative samples
                in if statistic < minimum_probability_threshold
                    then TestFailure $
                            printf "Computed Kolmogorov statistic was %f < %f."
                                statistic
                                minimum_probability_threshold
                    else TestOK
        }
-- @-node:gcross.20100109140101.1519:testDistribution
-- @+node:gcross.20100109130159.1286:testWalkDistribution
testWalkDistribution :: String -> (Double -> Double) -> Int -> Double -> IO a -> (a -> IO (Double,a)) -> Test
testWalkDistribution name computeExactCumulative number_of_tests minimum_probability_threshold makeSeed generator =
    Test name $
        TestCase
        {   testCount          = number_of_tests
        ,   testDataGenerator  = TestDataGenerator $ makeSeed >>= data_generator
        ,   testDataSeed       = [] :: [Double]
        ,   testDataProcessor  = \(!sample) (!previous_samples) -> sample:previous_samples
        ,   testDataSummarizer = \samples -> TestResult $
                let statistic = trace "summarizing" $ computeKolmogorovStatisticFromExact computeExactCumulative samples
                in if statistic < minimum_probability_threshold
                    then TestFailure $
                            printf "Computed Kolmogorov statistic was %f < %f."
                                statistic
                                minimum_probability_threshold
                    else TestOK
        }
  where
    data_generator = fmap (second (TestDataGenerator . data_generator)) . generator
-- @-node:gcross.20100109130159.1286:testWalkDistribution
-- @-node:gcross.20100107114651.1474:Interface
-- @-others
-- @-node:gcross.20100107191635.1412:@thin sources/Test/Framework/Providers/Statistics.hs
-- @-leo
