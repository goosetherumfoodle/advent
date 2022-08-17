
module FourteenTest (main) where

import Protolude hiding (Bits)
import Test.Hspec -- hiding (shouldBe)
-- import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ
import qualified Data.Map as M
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Positive(..))

import Fourteen

main :: IO ()
main = hspec $ do
  describe "run" $ do
    it "reads in all the values and sums everything in memory" $ do
      let
        input = [r|
mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
|]

        result = run input

      result `shouldBe` Right 208

  describe "parseInput'" $ do
    it "builds commands and bitmask from text input" $ do
      let
        input = [r|
mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
|]
        expectedCmds = [
            MaskInsert $ M.fromList [(0, FX), (1, FI), (4, FI), (5, FX)]
          , MemInsert (toBin 42) 100
          , MaskInsert $ M.fromList [(0, FX), (1, FX), (3, FX)]
          , MemInsert (toBin 26) 1
          ]

        result = parseInput input

      result `shouldBe` Right expectedCmds

  describe "toBin" $ do
    it "converts integers to binary" $ do
      toBin 10 `shouldBe` encBits [I,O,I,O]

  describe "toDec . toBin" $ do
    prop "can compose" $ do
      (\n -> getPositive n `shouldBe` (toDec . toBin $ getPositive n))

  describe "getAddrs" $ do
    it "uses the memory-insertion addr and the bitmask to determine addrs" $ do
      let
        mask = M.fromList [(3, FX), (1, FX), (0, FX)]
        inputAddr = toBin 26
        expected = sort $ fmap toBin [16, 17, 18, 19, 24, 25, 26, 27]

        result = sort $ getAddrs mask inputAddr

      result `shouldBe` expected

  describe "floatCartProd" $ do
    it "create cartesian products of the Xs in float bits" $ do
      let
        floats = [(5, FX), (3, FX), (1, FX)]
        expected =
          [
              [(5, O),(3, O),(1, O)]
            , [(5, O),(3, O),(1, I)]
            , [(5, O),(3, I),(1, O)]
            , [(5, O),(3, I),(1, I)]
            , [(5, I),(3, O),(1, O)]
            , [(5, I),(3, O),(1, I)]
            , [(5, I),(3, I),(1, O)]
            , [(5, I),(3, I),(1, I)]
          ]

      floatCartProd floats `shouldBe` expected
