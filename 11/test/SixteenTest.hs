module SixteenTest (main) where

import Protolude hiding (State, lookup)
import Test.Hspec
import Text.RawString.QQ
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Sixteen

main :: IO ()
main = hspec $ parallel $ do
  describe "sumInvalids'" $ do
    it "sums the invalids" $ do
      let rules = M.fromList [("valid in these ranges", ((9,11),(50,60)))
                             ,("or these",              ((3,4),(19,21)))]
          xs = [[1,1,10],[20,1,1]]

      sumInvalids' rules (fmap V.fromList xs) 0 `shouldBe` 4

  describe "parseInput" $ do
    it "reads in whole input" $ do
      let
        input = [r|
class: 1-3 or 5-7
row: 6-11 or 33-44
seat stuff: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
|]

        result = parseInput input

      result `shouldBe` Right (Input {
        rules =         M.fromList [
              ("class",      ((1,3),(5,7)))
            , ("row",        ((6,11),(33,44)))
            , ("seat stuff", ((13,40),(45,50)))
            ]
        , myTicket =  V.fromList [7,1,14]
        , nearbyTickets = [
              V.fromList [7,3,47]
            , V.fromList [40,4,50]
            , V.fromList [55,2,20]
            , V.fromList [38,6,12]
            ]
        })

--   describe "run" $ do
--     it "sums the invalid values" $ do
--       let
--         input = [r|
-- class: 1-3 or 5-7
-- row: 6-11 or 33-44
-- seat: 13-40 or 45-50

-- your ticket:
-- 7,1,14

-- nearby tickets:
-- 7,3,47
-- 40,4,50
-- 55,2,20
-- 38,6,12
-- |]

--         result = run input

--       result `shouldBe` Right 71

  describe "run" $ do
    it "lists the fields of your ticket, in order" $ do
      pending
      let
        input = [r|
class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
|]

        result = run input

      result `shouldBe` Right ["row", "class", "seat"]

  describe "shrinkFits" $ do
    it "shranks" $ do
      pending
      let
        tix = fmap Valid [
            vec [11,12,13]
          , vec [3,9,18]
          , vec [15,1,5]
          , vec [5,14,9]
          ]

        rules = [
          M.fromList [
          ("class", ((0,1),(4,19)))
          , ("row", ((0,5),(8,19)))
          , ("seat", ((0,13),(16,19)))
          ]
                , M.fromList [
          ("class", ((0,1),(4,19)))
          , ("row", ((0,5),(8,19)))
          , ("seat", ((0,13),(16,19)))
          ]
                ,     M.fromList [
          ("class", ((0,1),(4,19)))
          , ("row", ((0,5),(8,19)))
          , ("seat", ((0,13),(16,19)))
          ]
                ]

        result = shrinkFits tix rules

      result `shouldBe` ["row", "class", "seat"]

  describe "flipRowCol" $ do
    it "flips the row an column of each element" $  do
      let
        input = vec [
            vec ["a1", "a2", "a3"]
          , vec ["b1", "b2", "b3"]
          , vec ["c1", "c2", "c3"]
          ]

        result = flipRowCol input

        expected = vec [
            vec ["a1", "b1", "c1"]
          , vec ["a2", "b2", "c2"]
          , vec ["a3", "b3", "c3"]
          ]

      result `shouldBe` (Just expected)


vec = V.fromList
