module Fifteen (parseInput, run, run', speakNumbers, State(..), Lookup, PrimMonad, PrimState) where

import Protolude hiding (State, lookup)

import Protolude.Unsafe (unsafeTail, unsafeHead)
import Data.Either.Combinators (mapLeft)
import Data.Text (strip, splitOn, unpack, pack)
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.Primitive (PrimMonad, PrimState)

type TurnNumber = Int
type SpokenNumber = Int
type Lookup = MV.MVector (PrimState IO) Int

data ParseException = ParseException Text deriving Show

instance Exception ParseException

data State = S {
    currTurn :: TurnNumber
  , lastSpoken :: SpokenNumber
  , givens :: [SpokenNumber]
  , lookup :: MV.MVector (PrimState IO) Int
  }

run :: Text -> IO (Either Text Int)
run = runNth 2020

run' :: Text -> IO (Either Text Int)
run' = runNth 30000000

runNth :: Int -> Text -> IO (Either Text Int)
runNth nth input = do
  initNums <- either (throwIO . ParseException) pure $ parseInput input
  let initState = getInitState initNums
  lookup <- getInitLookup nth
  Right . lastSpoken <$> (speakNumbers nth (initState  lookup))

getInitLookup :: Int -> IO (MV.MVector (PrimState IO) Int)
getInitLookup x = MV.generate x (const 0)

parseInput :: Text -> Either Text [SpokenNumber]
parseInput =
  mapLeft pack .
  sequence .
  fmap (readEither . unpack . strip) .
  splitOn ","

getInitState :: [SpokenNumber] -> MV.MVector (PrimState IO) Int -> State
getInitState nums vec = S {
    currTurn = 1
  , lastSpoken = 0
  , givens = nums
  , lookup = vec
  }

-- if a starting number, speak it
-- if prev is first-time number, speak 0
-- if prev is already-spoken, speak diff between current turn and previous turn

speakNumbers :: Int -> State -> IO State
speakNumbers year st | year <= 0 = pure st
                            | (> 0) . length . givens $ st = do
                              (MV.write (lookup st) (lastSpoken st) (currTurn st))
                              let nextSpoken = unsafeHead . givens $ st
                              let nextGivens = unsafeTail . givens $ st
                              speakNumbers (year - 1) (nextState st nextSpoken nextGivens)
                            | otherwise = do
                                speaking <- (currTurn st -) <$> (lookup st `MV.read` (lastSpoken st))
                                lastTurn <- lookup st `MV.read` (lastSpoken st)
                                MV.write (lookup st) (lastSpoken st) (currTurn st)
                                if lastTurn /= 0 then
                                   speakNumbers (year - 1) (nextState st speaking (givens st))
                                else
                                   speakNumbers (year - 1) (nextState st 0 (givens st))

nextState :: State -> SpokenNumber -> [SpokenNumber] -> State
nextState st l g = S {
  currTurn   = currTurn st + 1
, lastSpoken = l
, givens     = g
, lookup     = lookup st
}
