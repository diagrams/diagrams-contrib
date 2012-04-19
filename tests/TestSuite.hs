-- | Main module to run all tests.
--
module Main where

import Test.Framework (defaultMain, testGroup)

import qualified  Diagrams.TwoD.Path.Turtle.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Diagrams.TwoD.Path.Turtle.Tests" Diagrams.TwoD.Path.Turtle.Tests.tests ]
