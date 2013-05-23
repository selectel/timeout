{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Timeout.Tests (tests) where

import Control.Exception (SomeException, try)
import Data.Maybe (isJust)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Property, Positive(..), suchThat, once,
                        within)
import Test.QuickCheck.Monadic (monadicIO, run, assert)

import Control.Timeout (timeout, sleep, msec)

-- | Interval that lesser than 1000000 microseconds, that guarantied by
-- 'Arbitrary' instance implementation.
newtype SmallInterval = SmallInterval Integer
    deriving (Show)

instance Arbitrary SmallInterval where
    arbitrary = fmap (SmallInterval . getPositive) $ suchThat arbitrary (< 1000)

-- | Timeout works with exceptions mechanism, so we need to check is
-- ordinary exceptions works.
testOtherException :: Property
testOtherException = monadicIO $ do
    res <- run $ try $ timeout 1 $ error "testOtherException"
    assert $ case res of
        Right _ -> False
        Left (_ :: SomeException) -> True

-- | Test is 'sleep' actually sleep.
testSleep :: SmallInterval -> Property
testSleep (SmallInterval interval) = monadicIO $ do
    res <- run $ do
        now <- getCurrentTime
        sleep t
        new <- getCurrentTime
        return $ diffUTCTime new now
    assert $ res > t
  where
    t = msec interval

tests :: Test
tests = testGroup "Control.Timeout.Tests"
    [ testProperty "timeout pass exceptions" $ once $ testOtherException
    , testProperty "sleep" testSleep
    ]
