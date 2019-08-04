module Common.Util.AesonHelpers where

dropPrefix_ :: String -> String
dropPrefix_ s = let suffix = drop 1 . dropWhile ('_' /=) $ s
                in if suffix == "" then s else suffix
