{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
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
