{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , TemplateHaskell
  , TypeOperators
  #-}

module ServantTest.HttpApi.User.ApiType where

import Servant.API
import Data.Aeson.TH

type UserAPI = ListUsersAPI
          :<|> GetUserAPI

type ListUsersAPI = QueryParam "sortBy" SortBy :> Get '[JSON] [User]
type GetUserAPI = Capture "userid" Integer :> Get '[JSON] User

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
