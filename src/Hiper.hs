{-# LANGUAGE OverloadedStrings #-}
module Hiper
    ( HiperConfig (..)
    ) where

import Data.IORef
import Data.Text
import Data.Map.Lazy as M

-- | Path to search for config files
type Path = Text

-- | Name of a config value
type Name = Text

-- | Extension of the config file
type Extension = Text

-- | Hiper configuration
data HiperConfig = HiperConfig {
    hcPaths :: [Path]
  , hcFile :: Name
  , hcExtensions :: [Extension]
  , hcDefaults :: M.Map Name Value
  }

-- | Values stored in the config
data Value = Bool Bool
           | String Text
           | Number Rational
           | List [Value]

-- | Hiper
data Hiper = Hiper {
    values :: IORef (M.Map Name Value)
  , hcConfig :: HiperConfig
  }

-- | Take declaration of how config should be loaded and load it
loadConfig :: HiperConfig -> IO Hiper
loadConfig c = do
  mapIORef <- newIORef M.empty
  -- load IORef with the default values

  -- check if there are ENV variables with the same names to take over defaults
  return $ Hiper mapIORef c

getConfigValue :: Hiper -> Name -> Maybe Value
getConfigValue = undefined
