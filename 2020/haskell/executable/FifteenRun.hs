module FifteenRun where

import Protolude
import Fifteen

main :: IO ()
main = print =<< Fifteen.run' "19,20,14,0,9,1"
