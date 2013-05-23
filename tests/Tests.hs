module Main where

import Test.Framework (defaultMain)

import qualified Control.Timeout.Tests

main :: IO ()
main = defaultMain
    [ Control.Timeout.Tests.tests
    ]
