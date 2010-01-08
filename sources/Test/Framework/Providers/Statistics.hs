-- @+leo-ver=4-thin
-- @+node:gcross.20100107191635.1412:@thin sources/Test/Framework/Providers/Statistics.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100107114651.1477:<< Language extensions >>
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20100107114651.1477:<< Language extensions >>
-- @nl

module Test.Framework.Providers.Statistics (testBinomial,computeTestCountForThresholds) where

-- @<< Import needed modules >>
-- @+node:gcross.20100107114651.1460:<< Import needed modules >>
import Debug.Trace

import Test.Framework.Providers.API

import Text.Printf

import Statistics.Distribution
import Statistics.Distribution.Normal
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
    ,   testDataGenerator  :: IO datum
    ,   testDataSeed       :: accumulator
    ,   testDataProcessor  :: accumulator -> datum -> accumulator
    ,   testDataSummarizer :: accumulator -> TestResult
    }

instance Testlike TestRunning TestResult (TestCase datum accumulator) where
    testTypeName _ = "Statistical Tests"
    runTest options test_case = runImprovingIO $ go number_of_points (testDataSeed test_case)
      where
        number_of_points = testCount test_case
        go :: Int -> accumulator -> ImprovingIO TestRunning TestResult TestResult
        go 0 accum = return $ testDataSummarizer test_case accum
        go n accum =
            yieldImprovement (TestRunning (number_of_points-n,number_of_points))
            >>
            liftIO (testDataGenerator test_case)
            >>=
            (go (n-1) . testDataProcessor test_case accum)
-- @-node:gcross.20100107114651.1465:TestCase
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
-- @-node:gcross.20100107114651.1466:Functions
-- @+node:gcross.20100107114651.1474:Interface
-- @+node:gcross.20100107114651.1469:testBinomial
testBinomial :: String -> Double -> Double -> Double -> IO Bool -> Test
testBinomial name probability_of_True mean_threshold minimum_probability_threshold generator =
    Test name $
        TestCase
        {   testCount          = number_of_tests
        ,   testDataGenerator  = generator
        ,   testDataSeed       = 0 :: Int
        ,   testDataProcessor  = \count result -> if result then count+1 else count
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
-- @-node:gcross.20100107114651.1469:testBinomial
-- @-node:gcross.20100107114651.1474:Interface
-- @-others
-- @-node:gcross.20100107191635.1412:@thin sources/Test/Framework/Providers/Statistics.hs
-- @-leo
