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

-- nums :: [Integer]
-- nums = 0 : recur 1
--   where
--     recur :: Integer -> [Integer]
--     recur n = n : recur (n + 1)

-- fibs :: [Integer]
-- fibs = 0 : recur 0 1
--   where
--     recur x y = x + y : recur y (x + y)

-- mults :: Integer -> [Integer]
-- mults n = 0 : recur 1
--   where
--     recur a = a * n : recur (a + 1)

-- shitzz :: Regex
-- shitzz = mkRegex "([0-9]+|x)"

-- legits :: [(Integer, Integer)]
-- legits = fmap ((,) <$> toInteger . fst <*> snd) $ catMaybes $ fmap sequence $ indexed $ fmap (readMaybe . unpack :: Text -> Maybe Integer) $ filter (/= "x") $ fmap mrMatch $ (fmap (match shitzz) (splitOn "," "13,x,x,x,x,x,x,37,x,x,x,x,x,461,x,x,x,x,x,x,x,x,x,x,x,x,x,17,x,x,x,x,19,x,x,x,x,x,x,x,x,x,29,x,739,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,x,x,x,23" :: [Text]) :: [MatchResult Text])

-- -- (\x -> (((a `div` x) + 1) * x) - a) ==

-- -- bus - (t `mod` bus)

-- -- 7,13,x,x,59,x,31,19
-- -- 0,01,2,3,04,5,06,07

-- dummies = [(0,7),(1,13),(4,59),(6,31),(7,19)]

-- horse :: [[Integer] -> [Integer]]
-- horse = fmap (\(off, bus) -> filter (\t -> trace (show t :: Text) $ ((((t `div` bus) + 1) * bus) - t) == off)) [(0,7),(1,13)]

-- goat = fmap (\(off, bus) -> filter (\t -> trace (show (off, bus, t) :: Text) $ ((((t `div` bus) + 1) * bus) - t) == off)) [(0,2),(1,3),(2,5)]

-- hawk :: [(Integer, Integer)] ->[[Integer] -> [Integer]]
-- hawk = fmap (\(off, bus) -> filter (\t -> (fuckMe t bus) == off))

-- fuckMe t a | t `mod` a == 0 = 0
--            | True           = a - (t `mod` a)

--  (0,2,10)

-- (2 - (10 `mod` 2))

-- ((((10 `div` 2) + 1) * 2) - 10)

-- doit a b = foldr ($) a b

-- 12,345,678

--   nums

-- 56,873,390
-- [1068780, 1068781, 1068782]

-- head $ foldr ($) nums (hawk legits)
