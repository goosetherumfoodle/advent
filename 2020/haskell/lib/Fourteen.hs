module Fourteen (
  Command(..)
  , Bits
  , Bit(..)
  , FloatBit(..)
  , run
  , parseInput
  , toBin
  , toDec
  , encBits
  , getAddrs
  , floatCartProd
  )
where

import Protolude hiding (Bits, State, state, mask, concat)
import Protolude.Unsafe (unsafeTail, unsafeHead)
import qualified Data.Map as M
import Data.Text (pack, unpack)
import Data.List.Index (indexed)
import qualified Text.Trifecta as Tri

-- >>> (fmap Fourteen.run . fmap Data.Text.pack . System.IO.hGetContents) =<< openFile "../14/input" ReadMode
-- Right 4160009892257

type Bits = Map Integer Bit

type MaskBits = Map Integer FloatBit

data FloatBit = FI | FX deriving (Show, Eq)

data Bit = I | O deriving (Show, Eq, Ord)

data Command =
    MemInsert Bits Integer
  | MaskInsert MaskBits
  deriving (Show, Eq)

data State = S {
    sMask :: MaskBits
  , sMem :: Map Integer Integer
  } deriving (Show)

initState :: State
initState = S { sMask =  M.empty, sMem = M.empty }

run :: Text -> Either Text Integer
run input = do
  cmds <- parseInput input
  let finalState = foldl' exec initState cmds
  pure . sum . M.elems . sMem $ finalState

exec :: State -> Command -> State
exec s (MaskInsert a)       = s{ sMask = a }
exec s (MemInsert addr val) = foldr memInsert s addrs
  where
    memInsert newAddr s' = s{ sMem = M.insert (toDec newAddr) val (sMem s') }
    addrs = getAddrs (sMask s) addr

unFloat :: [(a, FloatBit)] -> [(a, Bit)]
unFloat = catMaybes . (fmap.traverse) fn
  where
    fn FI = Just I
    fn _  = Nothing

getAddrs :: MaskBits -> Bits -> [Bits]
getAddrs mask addr =
  let ones = unFloat . filter (\(_, a) -> a == FI) . M.toList $ mask
      addr' = M.union (M.fromList ones) addr
  in
    fmap ((flip M.union) addr') . fmap M.fromList . floatCartProd $ M.assocs mask

floatCartProd :: [(Integer, FloatBit)] -> [[(Integer, Bit)]]
floatCartProd = sequence . expandBits . justXs
  where
    expandBits :: [(Integer, FloatBit)] -> [[(Integer, Bit)]]
    expandBits = foldr (\(i,_) bs -> [(i,O), (i,I)] : bs) []

    justXs = filter (\(_, f) -> f == FX)

parseInput :: Text -> Either Text [Command]
parseInput = eitherSuccess . Tri.parseString inputParser mempty . unpack

inputParser :: Tri.Parser [Command]
inputParser = Tri.whiteSpace >> (Tri.some (maskInsertParser <|> memInsertParser))
  where
    maskInsertParser = do
      _    <- Tri.string "mask = "
      bits <- some bitParser
      _    <- Tri.whiteSpace
      pure . MaskInsert . encFBits' $ bits

    memInsertParser = do
      _    <- Tri.string "mem["
      addr <- Tri.integer
      _    <- Tri.string "] = "
      val  <- Tri.integer
      _    <- Tri.whiteSpace
      pure $ MemInsert (toBin addr) val

bitParser :: Tri.Parser (Maybe FloatBit)
bitParser = do
  c <- Tri.oneOf "X10"
  case [c] of
    "1" -> pure $ Just FI
    "X" -> pure $ Just FX
    _   -> pure Nothing

eitherSuccess :: Tri.Result a -> Either Text a
eitherSuccess (Tri.Success a) = Right a
eitherSuccess (Tri.Failure a) = Left . pack . show $ a

toDec :: Bits -> Integer
toDec = sum . fmap raise . (fmap.fmap) funk . M.assocs
  where
    funk I = 2
    funk O = 0

    raise (0, 2) = 1
    raise (0, _) = 0
    raise (a, b) = b ^ a

toBin :: Integer -> Bits
toBin 0   = M.fromList [(0, O)]
toBin dec = M.fromList . indexed' . reverse . recur [] $ dec
  where
    recur :: [Bit] -> Integer -> [Bit]
    recur bits 0 = bits
    recur bits x | x `rem` 2 == 0 = recur (O : bits) (x `quot` 2)
                 | True           = recur (I : bits) (x `quot` 2)

indexed' :: [Bit] -> [(Integer, Bit)]
indexed' = fmap swap . (fmap.fmap) toInteger . fmap swap . indexed

encBits :: [Bit] -> Bits
encBits =
  foldr (\ (a, i) m -> M.insert i a m)  M.empty
  . (fmap.fmap) toInteger
  . fmap swap
  . indexed
  . reverse

encBits' :: [Maybe Bit] -> Bits
encBits' =
  foldr (\ (a, i) m -> M.insert i a m)  M.empty
  . (fmap.fmap) toInteger
  . fmap swap
  . catMaybes
  . fmap sequence
  . indexed
  . reverse

encFBits' :: [Maybe FloatBit] -> MaskBits
encFBits' =
  foldr (\ (a, i) m -> M.insert i a m)  M.empty
  . (fmap.fmap) toInteger
  . fmap swap
  . catMaybes
  . fmap sequence
  . indexed
  . reverse
