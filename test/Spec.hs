{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Prelude hiding (lookup)

import Data.Hiper

main :: IO ()
main = hspec $ do
  describe "Hiper defaults" $ do
    it "returns defaults when asked for defaults" $ do
      hiper <- loadConfig emptyConfig
      val <- lookup hiper "test" :: IO (Maybe Int)
      val `shouldBe` Nothing
