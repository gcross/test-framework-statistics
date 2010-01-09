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
import Debug.Trace

import Statistics.Distribution
import Statistics.Distribution.Normal

import System.Random

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
    -- @+node:gcross.20100107191635.1856:testBinomial
    ,testGroup "testBinomial" $
        -- @    @+others
        -- @+node:gcross.20100107191635.1858:mean = p = 0.5
        [testBinomial "mean = p = 0.5" 0.5 0.1 0.001 randomIO
        -- @nonl
        -- @-node:gcross.20100107191635.1858:mean = p = 0.5
        -- @+node:gcross.20100107191635.1860:mean = p = 0.5
        ,antiTest $ testBinomial "mean 0.4, p = 0.5" 0.4 0.1 0.001 randomIO
        -- @-node:gcross.20100107191635.1860:mean = p = 0.5
        -- @-others
        ]
    -- @-node:gcross.20100107191635.1856:testBinomial
    -- @+node:gcross.20100109130159.1287:testDistribution
    ,testGroup "testDistribution" $
        -- @    @+others
        -- @+node:gcross.20100109130159.1288:uniform / almost uniform
        [testDistribution "uniform" id 1000 0.001 randomIO
        ,antiTest $ testDistribution "almost uniform" (\x -> x**1.1) 10000 0.001 randomIO
        -- @-node:gcross.20100109130159.1288:uniform / almost uniform
        -- @+node:gcross.20100109130159.1290:linear / almost linear
        ,testDistribution "linear" (\x -> x*x) 1000 0.001 (fmap sqrt randomIO)
        ,antiTest $ testDistribution "almost linear" (\x -> x**2.1) 10000 0.001 (fmap sqrt randomIO)
        -- @-node:gcross.20100109130159.1290:linear / almost linear
        -- @-others
        ]
    -- @-node:gcross.20100109130159.1287:testDistribution
    -- @-others
    -- @-node:gcross.20100107191635.1611:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100107191635.1519:@thin tests/test.hs
-- @-leo
