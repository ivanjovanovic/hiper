{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Data.Text
import qualified Data.Map as M
import System.Directory
import System.IO
import System.Environment
import Prelude hiding (lookup)

import Data.Hiper
import Data.Hiper.Types

main :: IO ()
main = hspec $ do
  describe "Hiper defaults" $ do
    it "returns Nothing after lookup on empty config" $ do
      hiper <- loadConfig emptyConfig
      val <- lookup hiper "test" :: IO (Maybe Int)
      val `shouldBe` Nothing

    it "returns defaults when no other config present" $ do
      let Just config = addDefault emptyConfig "test" True
      hiper <- loadConfig config
      val <- lookup hiper "test" :: IO (Maybe Bool)
      val `shouldBe` Just True

  describe "Config file selection" $ do
    it "selects first config file found in provided config" $ do
      let config = emptyConfig
            { hcPaths = [pack "", pack "/tmp"]
            , hcFile = (pack "test")
            , hcExtensions = [pack "yaml"]
            }
      configFile <- configFilePath config
      configFile `shouldBe` Nothing
      withFile "/tmp/test.yaml" WriteMode (\_ -> return ())
      foundConfigFile <- configFilePath config
      foundConfigFile `shouldBe` Just "/tmp/test.yaml"
      removeFile "/tmp/test.yaml"

  describe "Yaml config parsing" $ do

    it "Should properly flatten the config" $ do
      let config = emptyConfig
            { hcPaths = [pack "./test/files"]
            , hcFile = (pack "test")
            , hcExtensions = [pack "yml"]
            }

      hiper <- loadConfig config
      firstLevel <- lookup hiper "firstLevel" :: IO (Maybe Int)
      firstLevel `shouldBe` Just 1
      secondLevel <- lookup hiper "second.level" :: IO (Maybe Int)
      secondLevel `shouldBe` Just 2
      thirdLevel <- lookup hiper "third.fourth.fifth" :: IO (Maybe Text)
      thirdLevel `shouldBe` Just "test string"
      arrayValue <- lookup hiper "sixt" :: IO (Maybe [Text])
      arrayValue `shouldBe` Just ["one", "two", "three"]

  describe "Default overloading" $ do
    it "should take from defaults if not in the file" $ do
      let config = emptyConfig
            { hcPaths = [pack "./test/files"]
            , hcFile = (pack "test")
            , hcExtensions = [pack "yml"]
            , hcDefaults = M.fromList [("seventh", Number 7)]
            }
      hiper <- loadConfig config
      seventh <- lookup hiper "seventh" :: IO (Maybe Int)
      seventh `shouldBe` Just 7

    it "should override defaults when value is in the file" $ do
      let config = emptyConfig
            { hcPaths = [pack "./test/files"]
            , hcFile = (pack "test")
            , hcExtensions = [pack "yml"]
            , hcDefaults = M.fromList [("firstLevel", Number 6)]
            }
      hiper <- loadConfig config
      firstLevel <- lookup hiper "firstLevel" :: IO (Maybe Int)
      firstLevel `shouldBe` Just 1

    it "should override any when in ENV variable" $ do
      let config = emptyConfig
            { hcPaths = [pack "./test/files"]
            , hcFile = (pack "test")
            , hcExtensions = [pack "yml"]
            , hcDefaults = M.fromList [("firstLevel", Number 6)]
            }
      hiper <- loadConfig config
      setEnv "firstLevel" "true"
      firstLevel <- lookup hiper "firstLevel" :: IO (Maybe Bool)
      firstLevel `shouldBe` Just True
      setEnv "second.level" "6"
      secondLevel <- lookup hiper "second.level" :: IO (Maybe Int)
      secondLevel `shouldBe` Just 6
