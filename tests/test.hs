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

import Test.HUnit
import Test.Framework
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
    -- @-others
    -- @-node:gcross.20100107191635.1611:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100107191635.1519:@thin tests/test.hs
-- @-leo