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

import System.Directory
import Control.Monad
import Data.IORef
import Data.Text hiding (take)
import Control.Monad (join)
import Prelude hiding (lookup, concat, FilePath)
import qualified Data.Map.Lazy as M
import Data.Hiper.Types.Internal
import Data.Hiper.Instances ()

-- | Path to search for config files
type Path = Text

-- | Name of a config value
type Name = Text

-- | Extension of the config file
type Extension = Text

type FilePath = Text

-- | Hiper configuration
data HiperConfig = HiperConfig {
    hcPaths :: [Path]
  , hcFile :: Name
  , hcExtensions :: [Extension]
  , hcDefaults :: M.Map Name Value
  }

-- | Provides an empty config to build on top.
-- Contains default list of supported extensions
emptyConfig :: HiperConfig
emptyConfig = HiperConfig
  { hcPaths = []
  , hcFile = ""
  , hcExtensions = ["json", "yaml"]
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
  mapIORef <- newIORef (hcDefaults c)
  -- Find configuration file from the configured paths


  -- check if there are ENV variables with the same names to take over defaults
  return $ Hiper mapIORef c

-- | lookup allows getting the value from the config registry.
-- It lets the caller specify return type
lookup :: Convertible a => Hiper -> Name -> IO (Maybe a)
lookup (Hiper values config) name = do
  valueMap <- readIORef values
  return $ join $ fmap convert (M.lookup name valueMap)

-- | Searches for configuration files and returns path
-- to the first one found according to configuration
-- Returns Nothing for non-complete configuration.
configFilePath :: HiperConfig -> IO (Maybe FilePath)
configFilePath (HiperConfig [] _ _ _) = return Nothing
configFilePath (HiperConfig _"" _ _) = return Nothing
configFilePath (HiperConfig _ _ [] _) = return Nothing
configFilePath (HiperConfig paths file extensions _) = do
  potentialFiles <- filterM (doesFileExist . unpack) (filePaths paths extensions file)
  case take 1 potentialFiles of
    [] -> return Nothing
    [f] -> return (Just f)

filePaths :: [Path] -> [Extension] -> Name -> [FilePath]
filePaths paths extensions name = do
  path <- paths
  extension <- extensions
  return $ concat [path, pack "/", name, pack ".", extension]
