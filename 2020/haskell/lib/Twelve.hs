{-# OPTIONS_GHC -Wwarn #-}

module Twelve where

import Protolude hiding (State)
import Text.Regex.TDFA (Regex, MatchResult(..), match)
import Data.Text (strip, pack, unpack, append)
-- import Data.List.Index (indexed)
import Util (mkRegex)

-- (fmap Twelve.run . fmap Data.Text.pack . System.IO.hGetContents) =<< openFile "../12/input" ReadMode

type Input = Text

data Command =
  NorthCmd Int
  | SouthCmd Int
  | EastCmd Int
  | WestCmd Int
  | LeftCmd Int
  | RightCmd Int
  | ForwardCmd Int
  deriving (Show, Eq)

data State = State
  {
    shipX :: Int
  , shipY :: Int
  , waypointX :: Int
  , waypointY :: Int
  }
  deriving (Show, Eq)

initState :: State
initState = State { shipX = 0, shipY = 0, waypointX = 10, waypointY = 1 }

run :: Input -> Either Text Int
run input = do
  cmds <- parseInput input
  pure . getManhattan . foldl' next initState $ cmds

next :: State -> Command -> State
next s (NorthCmd n)   = s{ waypointY = waypointY s + n }
next s (SouthCmd n)   = s{ waypointY = waypointY s - n }
next s (EastCmd n)    = s{ waypointX = waypointX s + n }
next s (WestCmd n)    = s{ waypointX = waypointX s - n }
next s (ForwardCmd n) = repeatFor moveToWaypoint s n
next s (LeftCmd n)    = repeatFor turnLeft s (turns n)
next s (RightCmd n)   = repeatFor turnRight s (turns n)

moveToWaypoint :: State -> State
moveToWaypoint s =
  s{
  shipX = waypointX s
  , shipY = waypointY s
  , waypointX = waypointX s + (waypointX s - shipX s)
  , waypointY = waypointY s + (waypointY s - shipY s)
  }

repeatFor :: (a -> a) -> a -> Int -> a
repeatFor fn init n = foldr (\_ a -> fn a) init (replicate n 0 :: [Int])

turnDirection :: (State -> State) -> State -> State
turnDirection turnFromCenter s =
  let
    xToCenter = shipX s
    yToCenter = shipY s
    centeredState = s{ shipX = 0
                     , shipY = 0
                     , waypointX = waypointX s - xToCenter
                     , waypointY = waypointY s - yToCenter
                     }
    turnedState = turnFromCenter centeredState
  in
    turnedState{
      shipX = xToCenter
    , shipY = yToCenter
    , waypointX = waypointX turnedState + xToCenter
    , waypointY = waypointY turnedState + yToCenter
    }

turnRight :: State -> State
turnRight = turnDirection turnRightFromCenter

turnLeft :: State -> State
turnLeft = turnDirection turnLeftFromCenter

turnLeftFromCenter :: State -> State
turnLeftFromCenter s = s{ waypointX = waypointY s * (-1), waypointY = waypointX s }

turnRightFromCenter :: State -> State
turnRightFromCenter s = s{ waypointX = waypointY s, waypointY = waypointX s * (-1) }

turns :: Int -> Int
turns deg = deg `div` 90 `mod` 4

commandRegex :: Regex
commandRegex = mkRegex "([FNSEWLR])([0-9]+)"

parseInput :: Text -> Either Text [Command]
parseInput = traverse getCmds . fmap (match commandRegex) . lines . strip

getCmds :: MatchResult Text -> Either Text Command
getCmds (MR _ _ _ ("F" : int : []) _) = toCmd ForwardCmd int
getCmds (MR _ _ _ ("N" : int : []) _) = toCmd NorthCmd int
getCmds (MR _ _ _ ("S" : int : []) _) = toCmd SouthCmd int
getCmds (MR _ _ _ ("E" : int : []) _) = toCmd EastCmd int
getCmds (MR _ _ _ ("W" : int : []) _) = toCmd WestCmd int
getCmds (MR _ _ _ ("L" : int : []) _) = toCmd LeftCmd int
getCmds (MR _ _ _ ("R" : int : []) _) = toCmd RightCmd int
getCmds (MR a b c _ _) = Left $ "could not parse: " `append` show (foldr append "" [a, b, c])

toCmd :: (Int -> Command) -> Text -> Either Text Command
toCmd cmd = fmap cmd . first pack . readEither . unpack

getManhattan :: State -> Int
getManhattan = (+) <$> abs . shipX <*> abs . shipY
