{-# LANGUAGE
    OverloadedStrings
  #-}
{- HLINT ignore "Redundant do" -}
module Main (main) where

import Test.Hspec

import qualified ServantTest.HttpApi.UserSpec
import qualified VersionSpec

main :: IO ()
main = hspec $ do
  VersionSpec.spec
  ServantTest.HttpApi.UserSpec.spec
