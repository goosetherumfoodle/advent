module TwelveTest (main) where

import Protolude
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ

import Twelve

main :: IO ()
main = hspec $ do
  describe "main" $ do
    it "calculates the manhattan distance of the final ship position" $ do
      let
        input = [r|
F10
N3
F7
R90
F11
|]
        result = run input
      result `shouldBe` Right 286

  describe "parseInput" $ do
    it "parses input text into list of commands" $ do
      let
        input = [r|
F10
N3
F7
R90
F11
|]
        expected = [
              ForwardCmd 10
              , NorthCmd 3
              , ForwardCmd 7
              , RightCmd 90
              , ForwardCmd 11
              ]
        result = parseInput input
      result `shouldBe` Right expected

  describe "moveToWaypoint" $ do
    it "moves the ship to the waypoint, and pushes out the waypoint" $ do
      let
        initState = State {
            shipX = 0
          , shipY = 0
          , waypointX = 10
          , waypointY = 1
          }
        expected = State {
            shipX = 10
          , shipY = 1
          , waypointX = 20
          , waypointY = 2
          }

        result = moveToWaypoint initState

      result `shouldBe` expected

  describe "turnLeft" $ do
    it "rotates the waypoint counterclockwise 90 deg around the ship" $ do
      let
        initState = State {
            shipX = 2
          , shipY = 2
          , waypointX = 3
          , waypointY = 3
          }
        expected = State {
            shipX = 2
          , shipY = 2
          , waypointX = 1
          , waypointY = 3
          }

        result = turnLeft initState

      result `shouldBe` expected

    context "with the waypoint on the same y-axis as the ship" $ do
      it "rotates the waypoint counterclockwise 90 deg around the ship" $ do
        let
          initState = State {
            shipX = 0
          , shipY = 0
          , waypointX = 3
          , waypointY = 0
          }
          expected = State {
            shipX = 0
          , shipY = 0
          , waypointX = 0
          , waypointY = 3
          }

          result = turnLeft initState

        result `shouldBe` expected
