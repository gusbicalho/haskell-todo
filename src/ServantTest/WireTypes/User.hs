{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ServantTest.WireTypes.User where

import Servant
import Data.Aeson.TH

data SortBy = Age | Name
instance FromHttpApiData SortBy where
  parseQueryParam text = case text of
    "age"  -> return Age
    "Age"  -> return Age
    "name" -> return Name
    "Name" -> return Name
    _ -> fail "Invalid SortBy param"

data User = User { id :: Integer
                 , name :: String
                 , age :: Int
                 , email :: String
                 } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
