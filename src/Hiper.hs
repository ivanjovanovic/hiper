{-# LANGUAGE OverloadedStrings #-}
module Hiper
    ( HiperConfig (..)
    ) where

import Data.IORef
import Data.Text
import Data.HashMap.Lazy as H

-- | Path to search for config files
type Path = Text

-- | Name of a config value
type Name = Text

-- | Extension of the config file
type Extension = Text

-- Hiper configuration
data HiperConfig = HiperConfig {
    hcPaths :: [Path]
  , hcFile :: Name
  , hcExtensions :: [Extension]
  }

-- | Values stored in the config
data Value = Bool Bool
           | String Text
           | Number Rational
           | List [Value]

-- | Hiper configuration
data Hiper = Hiper {
    values :: IORef (H.HashMap Name Value)
  , hcConfig :: HiperConfig
  }
