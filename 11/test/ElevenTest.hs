{-# OPTIONS_GHC  #-}

module ElevenTest (main) where

import Protolude
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ
import Data.Vector as V

import Eleven

main :: IO ()
main = hspec $ do
  describe "cartesian practice" $ do
    describe "cartTwo" $ do
      it "basics" $ do
        cartTwo [1,2] [3,4] `shouldBe` [[1,3], [1,4], [2,3], [2,4]]

      it "first empty" $ do
        cartTwo [] [3,4] `shouldBe` []

      it "second empty" $ do
        cartTwo [1,2] [] `shouldBe` []

      it "both empty" $ do
        cartTwo ([] :: [Int]) [] `shouldBe` []

    describe "cartAll" $ do
      it "basic again" $ do
        cartAll [[1,2], [3,4]] `shouldBe` [[1,3], [1,4], [2,3], [2,4]]

  describe "eleven" $ do
    it "counts the seats occupied after a consistent state is reached" $ do
      let
        input = [r|
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
|]

        result = run input

      result `shouldBe` Right 26

  describe "parseInput" $ do
    it "turns an input string into a seating map" $ do
      let input = [r|
L.##.
LLLLL
L.#.#
|]
          expected = Seats $ V.fromList [
                Row $ V.fromList [
                    SeatEmpty,
                    Floor,
                    SeatOccupied,
                    SeatOccupied,
                    Floor
                    ],
                Row $ V.fromList [
                    SeatEmpty,
                    SeatEmpty,
                    SeatEmpty,
                    SeatEmpty,
                    SeatEmpty
                    ],
                Row $ V.fromList [
                    SeatEmpty,
                    Floor,
                    SeatOccupied,
                    Floor,
                    SeatOccupied
                    ]
                ]

          result = parseInput input

      result `shouldBe` Right expected

  describe "nextState" $ do
    it "derives the next seating state" $ do
      let input = [r|
#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##LL.LL.L#
L.LL.LL.L#
#.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLL#.L
#.L#LL#.L#
|]
          expected = [r|
#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##L#.#L.L#
L.L#.#L.L#
#.L####.LL
..#.#.....
LLL###LLL#
#.LLLLL#.L
#.L#LL#.L#
|]
          result = nextState <$> parseInput input

      result `shouldBe` parseInput expected
