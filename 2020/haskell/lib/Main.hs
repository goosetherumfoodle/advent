{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (
  module Eleven
  ) where

--  Data.ByteString.Lazy LBS> (fmap eleven . fmap decodeUtf8 . fmap LBS.toStrict . hGetContents) =<< openFile "input" ReadMode

-- import Protolude

import Eleven as Eleven
