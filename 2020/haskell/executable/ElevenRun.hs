module ElevenRun where

import Protolude
import Eleven
import qualified Data.Text as Txt
import qualified System.IO as IO

main :: IO ()
main = ((=<<) print . fmap Eleven.run . fmap Txt.pack . IO.hGetContents) =<< openFile "./11/input" ReadMode
