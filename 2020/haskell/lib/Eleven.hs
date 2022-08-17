{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eleven (
  SeatingState(..)
  , Row(..)
  , Seat(..)
  , run
  , parseInput
  , nextState
  ) where

-- import System.IO (hGetContents)
-- import qualified Eleven as Eleven
-- (fmap Eleven.run . fmap Data.Text.pack . hGetContents) =<< openFile "input" ReadMode

import Protolude hiding (state)
import Protolude.Unsafe as US
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (unpack, strip, append)

type Input = Text
data Seat = SeatOccupied | SeatEmpty | Floor  deriving (Show, Eq)
newtype SeatingState = Seats (Vector Row)     deriving (Show, Eq, Hashable)
newtype Row = Row {getSeats :: (Vector Seat)} deriving (Show, Eq, Hashable)

instance Hashable a => Hashable (Vector a) where
  hashWithSalt salt = Protolude.sum . V.imap (\i a -> (hashWithSalt salt i) * (hashWithSalt salt a))

instance Hashable Seat where
  hashWithSalt salt seat = hashWithSalt salt (show seat :: [Char])

run :: Input -> Either Text Int
run input = do
  state <- parseInput input
  steady <- findTerminus 0 mempty state
  pure $ countOccupied steady

parseInput :: Input -> Either Text SeatingState
parseInput =
  fmap (Seats . V.fromList)
  . traverse buildRow
  . lines
  . strip

buildRow :: Text -> Either Text Row
buildRow  = fmap Row . fmap V.fromList . traverse parseSeat . unpack

parseSeat :: Char -> Either Text Seat
parseSeat c | c == (US.unsafeHead ".") = Right Floor
parseSeat c | c == (US.unsafeHead "L") = Right SeatEmpty
parseSeat c | c == (US.unsafeHead "#") = Right SeatOccupied
parseSeat c  = Left $ "couldn't parse character: " `append` show c

findTerminus :: Int -> Set Int -> SeatingState -> Either Text SeatingState
findTerminus prevHash seen state =
  let
    stateHash = hash state
  in
    if stateHash == prevHash
    then Right state
    else if Set.member stateHash seen
         then Left $ "loop detected after transformations: " `append` (show $ length seen)
         else findTerminus stateHash (Set.insert stateHash seen) (nextState state)

nextState :: SeatingState -> SeatingState
nextState seats@(Seats rows) = Seats $ V.imap (nextRowState seats) rows
  where
    nextRowState :: SeatingState -> Int -> Row -> Row
    nextRowState state i (Row r) = Row $ V.imap (nextSeatState state i) r

    nextSeatState :: SeatingState -> Int -> Int -> Seat -> Seat
    nextSeatState state rowI colI seat =
      let
        surrounding = getSurrounding state rowI colI
      in
        nextSeat seat surrounding
      where
        nextSeat :: Seat -> [Seat] -> Seat
        nextSeat SeatEmpty ss    | countOccupied'' ss == 0 = SeatOccupied
        nextSeat SeatOccupied ss | countOccupied'' ss >= 5 = SeatEmpty
        nextSeat s _ = s

countOccupied :: SeatingState -> Int
countOccupied (Seats s) = sum . V.map countOccupied' $ s

countOccupied' :: Row -> Int
countOccupied' = countOccupied'' . getSeats

countOccupied'' :: Foldable f => f Seat -> Int
countOccupied'' = length . filter ((==) SeatOccupied) . toList

getSurrounding :: SeatingState -> Int -> Int -> [Seat]
getSurrounding state rowI colI =
  let
    look = firstSeen state
    sc = SC {row = rowI, col = colI}
  in
    catMaybes $ fmap (look sc) allDirections

data SeatCoordinates = SC { col :: Int, row :: Int }
data Direction =
  ForwardLeftDirection
  | ForwardDirection
  | ForwardRightDirection
  | LeftDirection
  | RightDirection
  | BackLeftDirection
  | BackDirection
  | BackRightDirection

allDirections :: [Direction]
allDirections =
  [ ForwardLeftDirection
  , ForwardDirection
  , ForwardRightDirection
  , LeftDirection
  , RightDirection
  , BackLeftDirection
  , BackDirection
  , BackRightDirection
  ]

getSeat :: SeatingState -> SeatCoordinates -> Maybe Seat
getSeat (Seats rows) sc = (V.!? col sc) =<< (getSeats <$> (rows V.!? row sc))

firstSeen :: SeatingState -> SeatCoordinates -> Direction -> Maybe Seat
firstSeen s sc d = firstSeenIter (Just Floor) s sc
  where
    firstSeenIter :: Maybe Seat -> SeatingState -> SeatCoordinates -> Maybe Seat
    firstSeenIter (Just Floor) state sc' = firstSeenIter nextSeat state nextCoord
      where
        nextCoord = getNextCoord d sc'
        nextSeat = getSeat state nextCoord

    firstSeenIter found _ _ = found

getNextCoord :: Direction -> SeatCoordinates -> SeatCoordinates
getNextCoord ForwardLeftDirection sc  = SC {row = row sc - 1, col = col sc - 1 }
getNextCoord ForwardDirection sc      = SC {row = row sc - 1, col = col sc     }
getNextCoord ForwardRightDirection sc = SC {row = row sc - 1, col = col sc + 1 }
getNextCoord LeftDirection sc         = SC {row = row sc,     col = col sc - 1 }
getNextCoord RightDirection sc        = SC {row = row sc,     col = col sc + 1 }
getNextCoord BackLeftDirection sc     = SC {row = row sc + 1, col = col sc - 1 }
getNextCoord BackDirection sc         = SC {row = row sc + 1, col = col sc     }
getNextCoord BackRightDirection sc    = SC {row = row sc + 1, col = col sc + 1 }
