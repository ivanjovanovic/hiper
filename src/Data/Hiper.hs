{-# LANGUAGE OverloadedStrings #-}
module Data.Hiper
    (
      -- * Types
      HiperConfig (..)
    , Value(..) -- to be removed from interface

      -- * configuration of the config loader
    , emptyConfig
    , addDefault

      -- * config loader
    , loadConfig
    , lookup
    ) where

import Data.IORef
import Data.Text
import Control.Monad (join)
import Prelude hiding (lookup)
import qualified Data.Map.Lazy as M
import Data.Hiper.Types.Internal
import Data.Hiper.Instances ()

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

emptyConfig :: HiperConfig
emptyConfig = HiperConfig
  { hcPaths = []
  , hcFile = ""
  , hcExtensions = []
  , hcDefaults = M.empty
  }

-- | TODO: Remove exposure of the internal value
-- Currently you have to provide the Value so you have to know
-- the internals. It should be possible to convert the provided
-- value to internal representation.
addDefault :: HiperConfig -> Name -> Value -> HiperConfig
addDefault config name val =
  config {hcDefaults = M.insert name val (hcDefaults config)}


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

lookup :: Convertible a => Hiper -> Name -> IO (Maybe a)
lookup (Hiper values config) name = do
  valueMap <- readIORef values
  return $ join $ fmap convert (M.lookup name valueMap)
