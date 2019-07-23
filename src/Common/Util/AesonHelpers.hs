module Common.Util.AesonHelpers where

drop_prefix :: String -> String
drop_prefix = tail . dropWhile (not . ('_' ==))
