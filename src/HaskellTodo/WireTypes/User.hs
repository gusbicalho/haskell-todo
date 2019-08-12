{-# LANGUAGE TemplateHaskell #-}

{-|
Description: User-related types for serialization over the network

Types here should be used only in the external APIs exposed or consumed by this
service. In this case, they are used only in JSON serialization for the HTTP
API, but we could use the same types if we decided to send the same data to
a message queue server, for example.

Transforming between these types and the ones from "HaskellTodo.Models.User" is
performed by the functions in "HaskellTodo.Adapters.User".

Compare this approach to the one used in the database layer
("HaskellTodo.Db.User"). There, we decided to use thin wrappers around the
types from "HaskellTodo.Models.User", just so we could implement instances of
typeclasses needed to transform between the model and the database types. Here,
we decided to build fully new types, and create functions (adapters) dedicated
to transforming between them.

Part of the reason for this difference was to test and demonstrate the two
approaches. However, I also believe that decoupling is most important when
dealing with and exposing external APIs, which are contracts between separate
applications. We usually have way more control over our own database than over
clients of our API (or over APIs we consume).

I believe this means it is acceptable, in this case, to take the more
lightweight approach when dealing with the database, while using the more
heavy, boilerplate-y approach for our HTTP API.
-}
module HaskellTodo.WireTypes.User where

import Data.Aeson.TH
import qualified Data.Text as T
import Common.Util.AesonHelpers

data User = User {
  user_id :: Integer
, user_login :: T.Text
} deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropPrefix_ } ''User)

newtype SingleUser = SingleUser { user :: User } deriving (Eq, Show)
$(deriveJSON defaultOptions ''SingleUser)

newtype ManyUsers = ManyUsers { users :: [User] } deriving (Eq, Show)
$(deriveJSON defaultOptions ''ManyUsers)

data NewUserInput = NewUserInput {
  input_login :: T.Text
, input_password :: T.Text
} deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropPrefix_ } ''NewUserInput)
