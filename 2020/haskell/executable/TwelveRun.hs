module TwelveRun where

import Protolude
import Twelve
import qualified Data.Text as Txt
import qualified System.IO as IO

main :: IO ()
main = ((=<<) print . fmap Twelve.run . fmap Txt.pack . IO.hGetContents) =<< openFile "./12/input" ReadMode
