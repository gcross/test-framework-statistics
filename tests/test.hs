-- @+leo-ver=4-thin
-- @+node:gcross.20100107191635.1519:@thin tests/test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100107191635.1520:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
-- @-node:gcross.20100107191635.1520:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100107191635.1521:<< Import needed modules >>
import Control.Arrow

import Data.Number.Erf

import Debug.Trace

import Statistics.Distribution
import Statistics.Distribution.Normal

import System.Random.Mersenne

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.AntiTest
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.Statistics
import Test.QuickCheck
-- @-node:gcross.20100107191635.1521:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100107191635.1777:Functions
-- @+node:gcross.20100107191635.1778:echo
echo x = trace (show x) x
-- @-node:gcross.20100107191635.1778:echo
-- @+node:gcross.20100109140101.1522:metropolisStep
metropolisStep :: IO Double -> (Double -> Double) -> Double -> IO Double
metropolisStep generateNextStep computeDensity previous_step = do
    proposed_step <- generateNextStep
    toss <- randomIO
    return $
        if toss < computeDensity proposed_step / computeDensity previous_step
            then proposed_step
            else previous_step
-- @-node:gcross.20100109140101.1522:metropolisStep
-- @+node:gcross.20100109140101.1538:incrementalMetropolisStep
incrementalMetropolisStep :: IO Double -> (Double -> Double) -> Double -> IO Double
incrementalMetropolisStep generateNextStep computeDensity previous_step = do
    proposed_step <- fmap (previous_step+) generateNextStep
    toss <- randomIO
    return $
        if toss < computeDensity proposed_step / computeDensity previous_step
            then proposed_step
            else previous_step
-- @-node:gcross.20100109140101.1538:incrementalMetropolisStep
-- @-node:gcross.20100107191635.1777:Functions
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20100107191635.1611:<< Tests >>
    -- @+others
    -- @+node:gcross.20100107191635.1614:computeTestCountForThresholds
    [testProperty "computeTestCountForThresholds" $ do
        mean_threshold <- choose (0,1)
        probability_threshold <- choose(0,1)
        return $
            let number_of_tests = computeTestCountForThresholds mean_threshold probability_threshold
                variance_ = 1 / fromIntegral number_of_tests
            in cumulative (fromParams 0 variance_) (-(mean_threshold+0.001)) < probability_threshold
    -- @-node:gcross.20100107191635.1614:computeTestCountForThresholds
    -- @+node:gcross.20100107191635.1856:testBernoulli
    ,testGroup "testBernoulli" $
        -- @    @+others
        -- @+node:gcross.20100107191635.1858:mean = p = 0.5
        [testBernoulli "mean = p = 0.5" 0.5 0.1 0.001 randomIO
        -- @nonl
        -- @-node:gcross.20100107191635.1858:mean = p = 0.5
        -- @+node:gcross.20100107191635.1860:mean = p = 0.5
        ,antiTest $ testBernoulli "mean 0.4, p = 0.5" 0.4 0.1 0.001 randomIO
        -- @-node:gcross.20100107191635.1860:mean = p = 0.5
        -- @-others
        ]
    -- @-node:gcross.20100107191635.1856:testBernoulli
    -- @+node:gcross.20100109130159.1287:testDistribution
    ,testGroup "testDistribution" $
        -- @    @+others
        -- @+node:gcross.20100109130159.1288:uniform / almost uniform
        [testDistribution "uniform" id 1000 0.001 randomIO
        ,antiTest $ testDistribution "almost uniform" (\x -> x**1.1) 10000 0.001 randomIO
        -- @-node:gcross.20100109130159.1288:uniform / almost uniform
        -- @+node:gcross.20100109130159.1290:linear / almost linear
        ,testDistribution "linear" (\x -> x*x) 1000 0.001 (fmap sqrt randomIO)
        ,antiTest $ testDistribution "almost linear" (\x -> x**2.1) 20000 0.001 (fmap sqrt randomIO)
        -- @-node:gcross.20100109130159.1290:linear / almost linear
        -- @-others
        ]
    -- @-node:gcross.20100109130159.1287:testDistribution
    -- @+node:gcross.20100109140101.1523:testWalkDistribution
    ,testGroup "testWalkDistribution" $
        -- @    @+others
        -- @+node:gcross.20100109140101.1525:linear / almost linear
        [testWalkDistribution "linear" (\x -> x*x) 40000 0.001 (return 1) (fmap (id &&& id) . metropolisStep randomIO id)
        ,antiTest $ testWalkDistribution "almost linear" (\x -> x**2.1) 40000 0.001 (return 1) (fmap (id &&& id) . metropolisStep randomIO id)
        -- @-node:gcross.20100109140101.1525:linear / almost linear
        -- @+node:gcross.20100109140101.1527:quadratic / almost quadratic
        ,testWalkDistribution "quadratic" (\x -> x*x*x) 40000 0.001 (return 1) (fmap (id &&& id) . metropolisStep randomIO (\x -> x*x))
        ,antiTest $ testWalkDistribution "almost quadratic" (\x -> x**3.1) 40000 0.01 (return 1) (fmap (id &&& id) . metropolisStep randomIO (\x -> x*x))
        -- @-node:gcross.20100109140101.1527:quadratic / almost quadratic
        -- @-others
        ]
    -- @-node:gcross.20100109140101.1523:testWalkDistribution
    -- @+node:gcross.20100109140101.1536:stress test
    ,testWalkDistribution "stress test" erf 100000 0.001 (return 0) (fmap (id &&& id) . incrementalMetropolisStep (fmap ((0.5-).(*1)) randomIO) (\x -> exp(-x*x/2)))
    -- @-node:gcross.20100109140101.1536:stress test
    -- @-others
    -- @-node:gcross.20100107191635.1611:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100107191635.1519:@thin tests/test.hs
-- @-leo
