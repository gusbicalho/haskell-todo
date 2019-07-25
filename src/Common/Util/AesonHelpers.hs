module Common.Util.AesonHelpers where

dropPrefix_ :: String -> String
dropPrefix_ = tail . dropWhile ('_' /=)
