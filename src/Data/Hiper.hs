
{-# LANGUAGE OverloadedStrings #-}
module Data.Hiper
    (
      -- * Types
      HiperConfig (..)
    , Hiper (..)

      -- * configuration of the config loader
    , emptyConfig
    , addDefault

      -- * configuration properties
    , configFilePath

      -- * config loader
    , loadConfig
    , lookup
    ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString           as BS
import qualified Data.HashMap.Lazy         as HM
import           Data.IORef
import qualified Data.Map.Lazy             as M
import           Data.Maybe                (fromMaybe, listToMaybe)
import           Data.Text
import qualified Data.Vector               as V
import qualified Data.Yaml                 as Y
import           Prelude                   hiding (lookup)
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.FSNotify
import           System.IO                 hiding (FilePath)

import           Data.Hiper.Instances      ()
import           Data.Hiper.Parser
import           Data.Hiper.Types.Internal

type Name = Text

-- | Hiper configuration
data HiperConfig = HiperConfig {
    hcPaths      :: [FilePath]
  , hcFile       :: String
  , hcExtensions :: [String]
  , hcReload     :: Bool
  , hcDefaults   :: M.Map Name Value
  }

-- | Provides an empty config to build on top.
-- Contains default list of supported extensions
emptyConfig :: HiperConfig
emptyConfig = HiperConfig
  { hcPaths = []
  , hcFile = ""
  , hcExtensions = ["yaml"]
  , hcReload = False
  , hcDefaults = M.empty
  }

-- value to internal representation.
addDefault :: Configurable a => HiperConfig -> Name -> a -> Maybe HiperConfig
addDefault config name val =
  case toValue val of
    Just v ->  Just $ config {hcDefaults = M.insert name v (hcDefaults config)}
    _ -> Nothing

-- | Hiper
data Hiper = H { values   :: IORef (M.Map Name Value)
               , threadId :: Maybe ThreadId
               , hcConfig :: HiperConfig
               }

-- | Take declaration of how config should be loaded and load it
loadConfig :: HiperConfig -> IO Hiper
loadConfig c = do
  -- Find configuration file from the configured paths
  configFile <- configFilePath c
  configFileMap <- case configFile of
        Nothing -> return M.empty
        Just f  -> parseConfigFile f

  mapIORef <- newIORef $ M.union configFileMap (hcDefaults c)
  -- start thread watching for changes in the files
  let hiper = H {values = mapIORef, threadId = Nothing, hcConfig = c}

  threadHandle <-
    if hcReload c
    then do
      t <- forkIO (watchForChanges hiper)
      return $ Just t
    else return Nothing

  return $ hiper { threadId = threadHandle }

-- | parseConfigFile parses provided file
parseConfigFile :: FilePath -> IO (M.Map Name Value)
parseConfigFile f = do
  fileContents <- withFile f ReadMode BS.hGetContents
  return $ maybe M.empty foldValueToMap (Y.decode fileContents)

-- | Given the root of the name, return map with all keys namespaced by the root.
-- Namespacing is dot separated @root.name@
namespaceMap :: Name -> M.Map Name a -> M.Map Name a
namespaceMap name m = M.mapKeys (append (append name ".") ) m

foldValueToMap :: Y.Value -> M.Map Name Value
foldValueToMap (Y.Object hm) = M.foldrWithKey f M.empty $ M.fromList (HM.toList hm)
  where
    f :: Name -> Y.Value -> M.Map Name Value -> M.Map Name Value
    f k a result = M.union (parse k a) result

    parse :: Name -> Y.Value -> M.Map Name Value
    parse n o@(Y.Object _) = namespaceMap n (foldValueToMap o)
    parse n (Y.Array v) = case V.head v of
      Y.Object _ -> M.empty -- will ignore array of objects
      Y.Array _  -> M.empty -- will ignore array of arrays
      _          -> M.fromList [(n, List $ fmap convertValue (V.toList v))]
    parse n (Y.String v) = M.fromList [(n, String v)]
    parse n (Y.Number v) = M.fromList [(n, Number v)]
    parse n (Y.Bool v) = M.fromList [(n, Bool v)]
    parse n Y.Null = M.fromList [(n, Null)]
foldValueToMap _ = M.empty

convertValue :: Y.Value -> Value
convertValue (Y.String v) = String v
convertValue (Y.Number v) = Number v
convertValue (Y.Bool v)   = Bool v
convertValue Y.Null       = Null
convertValue _            = Null

-- | lookup allows getting the value from the config registry.
-- It lets the caller specify return type
lookup :: Configurable a => Hiper -> Name -> IO (Maybe a)
lookup hiper name = do
  valueMap <- readIORef $ values hiper
  -- check if there is ENV variable set for this configuration parameter
  envValue <- fmap (maybe Nothing parseEnv)(lookupEnv (unpack name))
  case envValue of
    Nothing -> return $ join $ fmap fromValue (M.lookup name valueMap)
    Just v  -> return $ fromValue v

parseEnv :: String -> Maybe Value
parseEnv s = case parseEnvVal s of
  Left _    -> Nothing
  Right val -> Just val

-- | Searches for configuration files and returns path
-- to the first one found according to configuration
-- Returns Nothing for non-complete configuration.
configFilePath :: HiperConfig -> IO (Maybe FilePath)
configFilePath (HiperConfig [] _ _ _ _) = return Nothing
configFilePath (HiperConfig _"" _ _ _) = return Nothing
configFilePath (HiperConfig _ _ [] _ _) = return Nothing
configFilePath (HiperConfig paths file extensions _ _) =
  listToMaybe <$> filterM doesFileExist (filePaths paths extensions file)

filePaths :: [String] -> [String] -> String -> [FilePath]
filePaths paths extensions name = do
  path <- paths
  extension <- extensions
  return $ path </> name <.> extension

watchForChanges :: Hiper -> IO ()
watchForChanges h = withManager $ \mgr -> do
  configFile <- configFilePath $ hcConfig h
  let file = fromMaybe "." configFile

  putStrLn $ "watching dir: " ++ show (takeDirectory file)

  void $ watchDir
    mgr
    (takeDirectory file)
    (const True)
    (reloadFromFile h)

  putStrLn "Watching configuration files for reload every 5 seconds"
  forever $ threadDelay 5000000

reloadFromFile :: Hiper -> Action
reloadFromFile h _ = do
  putStrLn "Reloading configuration"
  -- Find configuration file from the configured paths
  configFile <- configFilePath $ hcConfig h
  configFileMap <- case configFile of
    Nothing -> return M.empty
    Just f  -> parseConfigFile f

  atomicModifyIORef' (values h) $ \existingMap -> (M.union configFileMap existingMap, ())
