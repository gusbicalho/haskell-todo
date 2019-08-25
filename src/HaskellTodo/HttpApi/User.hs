{-|
Description: Servant API for dealing with Users

This implements a simple Servant API for creating and getting Users, and also
to get all Items for a User. This module should have no substantial code. All
we do here is:

1) Run authorization checks, by delegating to "Auth.Logic";

2) Run adapters to convert between our @WireTypes@ and @Models@; and

3) Call controllers to actually get some data or effect some change.

The 'server' must in a 'Monad' that satisfies all 'ServerConstraints'. One of
these is @'MonadReader' 'Env.Env'@. In other words, this API is coupled to the
concrete 'Env.Env' type we built for this application. For more discussion on
this, check out the docs for "HaskellTodo.Env".
-}
module HaskellTodo.HttpApi.User
  ( api
  , server
  , API
  , ServerConstraints
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Servant
import Servant.Auth.Server
import HaskellTodo.Auth.Types (IdentityTokenClaims)
import qualified HaskellTodo.Auth.Logic as Auth.Logic
import qualified HaskellTodo.Env as Env
import qualified HaskellTodo.WireTypes.User as Wire.User
import qualified HaskellTodo.Controllers.User as C.User
import qualified HaskellTodo.Adapters.User as A.User
import qualified HaskellTodo.WireTypes.Item as Wire.Item
import qualified HaskellTodo.Controllers.Item as C.Item
import qualified HaskellTodo.Adapters.Item as A.Item

type API = ReqBody '[JSON] Wire.User.NewUserInput :> Post '[JSON] Wire.User.SingleUser
      :<|> Capture "userid" Integer :> Get '[JSON] Wire.User.SingleUser
      :<|> Capture "userid" Integer :> "items" :>
        Get '[JSON] Wire.Item.ManyItems

api :: Proxy API
api = Proxy

type ServerConstraints m = ( MonadError ServerError m
                           , MonadIO m
                           , MonadReader Env.Env m
                           )

server :: ServerConstraints m => AuthResult IdentityTokenClaims -> ServerT API m
server auth = createUser
         :<|> getUser
         :<|> userItems
  where -- Handlers
    createUser newUserInput
      | Auth.Logic.authenticated auth = throwError err403
      | otherwise = do
          env <- ask
          maybeUser <- C.User.createUser (A.User.inputToNewUser newUserInput) env
          case maybeUser of
            Nothing -> throwError err500
            Just user -> return $ A.User.singleWire user

    getUser idParam
      | not $ Auth.Logic.authenticatedAsUser idParam auth = throwError err403
      | otherwise = do
          env <- ask
          maybeUser <- C.User.getUser idParam env
          result maybeUser
        where
          result Nothing  = throwError err404
          result (Just x) = return $ A.User.singleWire x

    userItems userIdParam
      | not $ Auth.Logic.authenticatedAsUser userIdParam auth = throwError err403
      | otherwise = getItems
        where
          getItems = do
            env <- ask
            items <- C.Item.findItemsByUserId userIdParam env
            return $ A.Item.manyWire items
