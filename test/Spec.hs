{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Hiper

main :: IO ()
main = hspec $ do
  describe "Hiper defaults" $ do
    it "returns defaults when asked for defaults" $ do
      v <- parseDefault (1 :: Int) (HiperConfig "test")
      v `shouldBe` (1 :: Int)
