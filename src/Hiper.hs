{-# LANGUAGE OverloadedStrings #-}
module Hiper
    ( HiperConfig (..)
    , parseDefault
    ) where

-- Configure configuration sources for Hiper configuration
data HiperConfig = HiperConfig {
  fromFile :: String
  }

parseDefault :: a -> HiperConfig -> IO a
parseDefault d c = return d
