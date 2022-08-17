module Util (mkRegex) where

import Protolude
import Text.Regex.TDFA (Regex, ExecOption(..), makeRegexOpts, defaultCompOpt)


mkRegex :: Text -> Regex
mkRegex = makeRegexOpts defaultCompOpt (ExecOption True)
