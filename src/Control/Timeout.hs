-- |
-- Module     : Control.Timeout
-- Copyright  : (c) Selectel
-- License    : MIT
-- Maintainer : Fedor Gogolev <knsd@knsd.net>
-- Stability  : unstable
--
-- This module provides simple interface for 'IO' time operations.
--
-- Example:
--
-- > module Main where
-- >
-- > import Control.Timeout (timeout, sleep)
--
-- > main :: IO ()
-- > main = do
-- >     timeout 1 $ sleep 2  -- Will return IO Nothing
-- >     timeout 2 $ sleep 1  -- Will return IO (Just ())
-- >     return ()
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Timeout
    ( NominalDiffTime
    , timeout
    , sleep
    , sec
    , msec
    , usec
    ) where

import Control.Exception (Exception)
import Control.Concurrent (myThreadId, forkIO, killThread, threadDelay, throwTo)
import Data.Typeable (Typeable)
import Data.Time.Clock (NominalDiffTime)
import Data.Unique (Unique, newUnique)

import Control.Monad.Catch (MonadCatch(..), bracket, handleJust)
import Control.Monad.Trans (MonadIO, liftIO)

-- | Exception used for timeout handling
newtype Timeout = Timeout Unique
    deriving (Eq, Typeable)

instance Show Timeout where
    show _ = "<<timeout>>"

instance Exception Timeout

-- | Time modifier, @sec 5@ means 5 seconds.
sec :: Integer -> NominalDiffTime
sec = fromInteger

-- | Time modifier, @msec 5@ means 5 milliseconds.
msec :: Integer -> NominalDiffTime
msec = (/ 1000) . fromInteger

-- | Time modifier, @usec 5@ means 5 microseconds.
usec :: Integer -> NominalDiffTime
usec = (/ 1000000) . fromInteger

-- | Try to execute any 'MonadIO' 'a' action in timeout.
-- If timeout occured — return 'Nothing' else 'Just' a.
timeout :: (MonadCatch m, MonadIO m) => NominalDiffTime -> m a -> m (Maybe a)
timeout t f | t <= 0 = return Nothing
            | otherwise = do
    pid <- liftIO myThreadId
    ex  <- liftIO newUnique >>= return . Timeout
    handleJust (\e -> if e == ex then Just () else Nothing)
               (\_ -> return Nothing)
               (bracket (liftIO $ forkIO (sleep t >> throwTo pid ex))
                        (liftIO . killThread)
                        (\_ -> f >>= return . Just))

-- | Sleep for 'NominalDiffTime', example:
--
-- > sleep 5  -- Will sleep for 5 seconds
-- > sleep $ usec 5  -- Will sleep for 5 microseconds
sleep :: (MonadIO m) => NominalDiffTime -> m ()
sleep = liftIO . threadDelay . floor . (* 1000000) . toRational
