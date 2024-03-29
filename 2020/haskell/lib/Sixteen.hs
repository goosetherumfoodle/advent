module Sixteen (ValidTix(..), Input(..), run, run', sumInvalids', parseRules, parseNearby, parseMyTicket, parseInput, shrinkFits, shrinkFits', flipRowCol, shrinkFitCol) where

import Protolude hiding (State)

import Protolude.Unsafe (unsafeHead, unsafeIndex)
import qualified Data.Map.Strict as M
import qualified Text.Trifecta as Tri
import qualified Data.Text as T
import Data.Text (pack, unpack)
import Data.Ix (inRange)
import qualified Data.Vector as V
import Data.Vector (Vector, imap)
import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector.Mutable as MV
import Control.Monad.Primitive (PrimState)

type Rules = Map Text ((Int, Int), (Int, Int))
type Ticket = Vector Int

data Input = Input {
        rules :: Rules
        , myTicket :: Ticket
        , nearbyTickets :: [Ticket]
        } deriving (Show, Eq)

-- >>> (fmap Sixteen.run . fmap pack . System.IO.hGetContents) =<< openFile "../16/input" ReadMode
-- Right 22057

eitherSuccess :: Tri.Result a -> Either Text a
eitherSuccess (Tri.Success a) = Right a
eitherSuccess (Tri.Failure a) = Left . pack . show $ a

parseInput :: Text -> Either Text Input
parseInput a = Input <$> parseRules a <*> parseMyTicket a <*> parseNearby a

parseRules :: Text -> Either Text Rules
parseRules = eitherSuccess . Tri.parseString ruleParser mempty . unpack

ruleParser :: Tri.Parser Rules
ruleParser =
  (fmap mconcat $ Tri.spaces *> Tri.manyTill (M.fromList <$> ruleParser) (Tri.try (Tri.string "your ticket")))
  where
    ruleParser = do
      ruleName <- Tri.try $ Tri.some (Tri.letter <|> Tri.try Tri.space) <* Tri.colon
      Tri.spaces
      firstMin <- fromInteger <$> Tri.integer
      Tri.char (T.head "-")
      firstMax <- fromInteger <$> Tri.integer
      Tri.spaces >> Tri.string "or" >> Tri.spaces
      secondMin <- fromInteger <$> Tri.integer
      Tri.char . T.head $  "-"
      secondMax <- fromInteger <$>Tri.integer
      return [(pack ruleName, ((firstMin,firstMax),(secondMin,secondMax)))]

parseNearby :: Text -> Either Text [Ticket]
parseNearby = eitherSuccess . Tri.parseString nearbyParser mempty . unpack

nearbyParser :: Tri.Parser [Ticket]
nearbyParser = do
  Tri.manyTill Tri.anyChar (Tri.string "nearby tickets:")
  Tri.spaces
  nearby <- Tri.manyTill (Tri.commaSep Tri.integer) Tri.eof
  return . fmap V.fromList . (fmap.fmap) fromInteger $ nearby

parseMyTicket :: Text -> Either Text Ticket
parseMyTicket = eitherSuccess . Tri.parseString myTicketParser mempty . unpack

myTicketParser :: Tri.Parser Ticket
myTicketParser = do
      Tri.manyTill Tri.anyChar (Tri.string "your ticket:")
      Tri.spaces
      myTicket' <- Tri.commaSep Tri.integer
      return . V.fromList . fmap fromInteger $ myTicket'

run :: Text -> Either Text [Text]
run = fmap decodeMyTicket . parseInput

valid :: [((Int, Int), (Int, Int))] -> Int -> Bool
valid [] _           = False
valid ((r1,r2):rs) a = inRange r1 a || inRange r2 a || valid rs a

justInvalids :: Map a ((Int, Int), (Int, Int)) -> Ticket -> Ticket
justInvalids rules' = V.filter $ not . valid (fmap snd . M.toList $ rules')

sumInvalids :: Input -> Int
sumInvalids inp = sumInvalids' (rules inp) (nearbyTickets inp) 0

sumInvalids' :: Rules -> [Ticket] -> Int -> Int
sumInvalids' _ [] acc = acc
sumInvalids' rules' (a:as) acc = sumInvalids' rules' as sum'
  where
    sum' = (+ acc) . foldr (+) 0 . (justInvalids rules') $ a

newtype ValidTix a = Valid { runValid :: a }
newtype InvalidTix a = Invalid a

run' :: Text -> Either Text [Text]
run' = fmap decodeMyTicket . parseInput

decodeMyTicket :: Input -> [Text]
decodeMyTicket input =
  let
    valids = Valid (myTicket input) : justValids (rules input) (nearbyTickets input)
    fits = generateFits (rules input) (myTicket input)
  in
    shrinkFits valids fits

shrinkFits :: [ValidTix Ticket] -> [Rules] -> [Text]
shrinkFits = (fmap.fmap) unsafeHead . shrinkFits'

shrinkFits' :: [ValidTix Ticket] -> [Rules] -> [[Text]]
shrinkFits' tx rs = V.toList $ V.imap shrinkCols ticketCols
  where
    shrinkCols ti c = fmap fst . shrinkFitCol c . M.toList $ unsafeIndex rs ti
    ticketCols = flipRowCol . V.fromList . fmap runValid $ tx

shrinkFitCol :: Ticket -> [(Text, ((Int,Int),(Int,Int)))] -> [(Text, ((Int,Int),(Int,Int)))]
shrinkFitCol t rs = filter flt rs
  where
    flt :: (Text, ((Int,Int),(Int,Int))) -> Bool
    flt (_,rs') = and $ V.map (\t' -> inRange (fst rs') t' || inRange (snd rs') t') t

justValids :: Rules -> [Ticket] -> [ValidTix Ticket]
justValids rs = (fmap.fmap) Valid $ filter (\t -> null $ justInvalids rs t)

generateFits :: Rules -> Ticket -> [Rules]
generateFits rs = V.toList . V.map (const rs)

flipRowCol :: Show a => Vector (Vector a) -> Vector (Vector a)
flipRowCol a = V.imap (\ r -> imap (\ c x -> (a V.! c) V.! r)) newVecs
  where
    newVecs = V.replicate (V.length (a V.! 1)) (V.replicate (V.length a) ((a V.! 0) V.! 0))
