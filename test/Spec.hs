{-# LANGUAGE
    OverloadedStrings
  #-}
{- HLINT ignore "Redundant do" -}
module Main (main) where

import Test.Hspec

import qualified ServantTest.HttpApi.UserSpec

main :: IO ()
main = hspec $ do
  ServantTest.HttpApi.UserSpec.spec
