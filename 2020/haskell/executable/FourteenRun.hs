module FourteenRun where

import Protolude
import Fourteen
import qualified System.IO as IO
import qualified Data.Text as Txt

main :: IO ()
main = ((=<<) print . fmap Fourteen.run . fmap Txt.pack . IO.hGetContents) =<< openFile "./14/input" ReadMode
-- Right 4160009892257
