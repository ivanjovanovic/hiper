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

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
