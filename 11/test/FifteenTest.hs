module FifteenTest (main) where

import Protolude hiding (State, lookup)
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Fifteen

main :: IO ()
main = hspec $ do
  describe "parseInput" $ do
    it "reads in initial numbers" $ do
      let
        input = "0,5,2,2"

        result = parseInput input

      result `shouldBe` Right [0,5,2,2]

  describe "run" $ do
    it "reads in all the values and gives the 2020th number spoken" $ join $ do
      let
        input = "0,3,6"

        result = run input

      shouldBe <$> result <*> pure (Right 436)

  describe "speakNumbers" $ do
    it "if number is a given, speak it" $ join $ do
        lookup <- genVec [0]
        let initState = S {
          currTurn = 1
          , lastSpoken = 0
          , givens = [1]
          , lookup = lookup
          }

        let result = speakNumbers 1 initState

        let expected = S {
          currTurn = 2
          , lastSpoken = 1
          , givens = []
          , lookup = lookup
          }

        shouldBe <$> (showable =<< result) <*> (showable expected)

    it "if the last number spoken hadn't been seen before, speak 0" $ join $ do
        lookup <- genVec [0,0,0,0,0,0,0,0,0,0,0]
        let initState = S {
          currTurn = 1
          , lastSpoken = 10
          , givens = []
          , lookup = lookup
          }

        let result = speakNumbers 1 initState

        let expected = S {
          currTurn = 2
          , lastSpoken = 0
          , givens = []
          , lookup = lookup
          }
        shouldBe <$> (showable =<< result) <*> (showable expected)

    it "if the last number spoken has been seen before, speak diff between turns" $ join $ do
        lookup <- genVec [0,0,0,0,0,3]
        let initState = S {
            currTurn = 10
          , lastSpoken = 5
          , givens = []
          , lookup = lookup
          }

        let result = speakNumbers 1 initState

        let expected = S {
              currTurn = 11
            , lastSpoken = 7
            , givens = []
            , lookup = lookup
            }
        shouldBe <$> (showable =<< result) <*> (showable expected)

data ShowableState = SS {
    currTurns :: Int
  , lastSpokens :: Int
  , givenss :: [Int]
  , lookups :: V.Vector Int
  }
  deriving (Show, Eq)

showable :: State -> IO ShowableState
showable s = SS (currTurn s) (lastSpoken s) (givens s) <$> (fmap V.convert . UV.freeze $ Fifteen.lookup s)

genVec :: [Int] -> IO Lookup
genVec xs = UV.thaw . V.convert $ (V.fromList xs)
