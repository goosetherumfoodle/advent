{-# OPTIONS_GHC  #-}

module AdventTest where

import Protolude

import qualified ElevenTest
import qualified TwelveTest
import qualified FourteenTest
import qualified FifteenTest
import qualified SixteenTest

main :: IO ()
main = do
  ElevenTest.main
  TwelveTest.main
  FourteenTest.main
  FifteenTest.main
  SixteenTest.main
