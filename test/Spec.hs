{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Data.Text
import System.Directory
import System.IO
import Prelude hiding (lookup)

import Data.Hiper

main :: IO ()
main = hspec $ do
  describe "Hiper defaults" $ do
    it "returns Nothing after lookup on empty config" $ do
      hiper <- loadConfig emptyConfig
      val <- lookup hiper "test" :: IO (Maybe Int)
      val `shouldBe` Nothing

    it "returns defaults when no other config present" $ do
      let config = addDefault emptyConfig "test" (Bool True)
      hiper <- loadConfig config
      val <- lookup hiper "test" :: IO (Maybe Bool)
      val `shouldBe` Just True

  describe "Config file selection" $ do
    it "selects first config file found in provided config" $ do
      let config = emptyConfig
            { hcPaths = [pack "", pack "/tmp"]
            , hcFile = (pack "test")
            , hcExtensions = [pack "json"]
            }
      configFile <- configFilePath config
      configFile `shouldBe` Nothing
      withFile "/tmp/test.json" WriteMode (\_ -> return ())
      foundConfigFile <- configFilePath config
      foundConfigFile `shouldBe` Just "/tmp/test.json"
      removeFile "/tmp/test.json"
