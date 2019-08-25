{-|
Description: Servant API for dealing with Items

This implements a simple Servant API for creating and getting Items. This
module should have no substantial code. All we do here is:

1) Run authorization checks, by delegating to "Auth.Logic";

2) Run adapters to convert between our @WireTypes@ and @Models@; and

3) Call controllers to actually get some data or effect some change.

The 'server' must in a 'Monad' that satisfies all 'ServerConstraints'. One of
these is @'MonadReader' 'Env.Env'@. In other words, this API is coupled to the
concrete 'Env.Env' type we built for this application. For more discussion on
this, check out the docs for "HaskellTodo.Env".

I wanted to pattern match on the authorization logic right at the top of the
'server', which simplified to code for the specific handlers. Howver, this lead
to a small boilerplate of "forbidden handlers" at the bottom, which must match
the shape of the API, even though they just respond 403. I haven't found a
better way yet.
-}
module HaskellTodo.HttpApi.Item where

import Control.Monad.Except
import Control.Monad.Reader
import Servant
import Servant.Auth.Server
import HaskellTodo.Auth.Types (IdentityTokenClaims)
import qualified HaskellTodo.Auth.Logic as Auth.Logic
import HaskellTodo.Env
import qualified HaskellTodo.WireTypes.Item as Wire.Item
import qualified HaskellTodo.Controllers.Item as C.Item
import qualified HaskellTodo.Adapters.Item as A.Item

type API = ReqBody '[JSON] Wire.Item.NewItemInput :> Post '[JSON] Wire.Item.SingleItem
      :<|> Capture "itemid" Integer :> (
                Get '[JSON] Wire.Item.SingleItem
           :<|> Delete '[JSON] Wire.Item.SingleItem
           :<|> ReqBody '[JSON] Wire.Item.ItemUpdateInput :> Put '[JSON] Wire.Item.SingleItem
           )

api :: Proxy API
api = Proxy

type ServerConstraints m = ( MonadError ServerError m
                           , MonadIO m
                           , MonadReader Env m
                           )

justOr404 :: MonadError ServerError m => Maybe a -> m a
justOr404 Nothing     = throwError err404
justOr404 (Just item) = return item

server :: ServerConstraints m => AuthResult IdentityTokenClaims -> ServerT API m
server (Auth.Logic.authenticatedUserId -> Just userId) =
         createItem
    :<|> \itemIdParam -> getItem itemIdParam
                    :<|> deleteItem itemIdParam
                    :<|> updateItem itemIdParam
  where -- handlers
    createItem newItemInput
      | Wire.Item.input_userId newItemInput /= userId = throwError err403
      | otherwise = do
          env <- ask
          item <- C.Item.createItem (A.Item.inputToNewItem newItemInput) env
          return $ A.Item.singleWire item

    getItem itemIdParam = do
      env <- ask
      maybeItem <- C.Item.getItemBelongingToUserId itemIdParam userId env
      A.Item.singleWire <$> justOr404 maybeItem

    deleteItem itemIdParam = do
      env <- ask
      maybeItem <- C.Item.deleteItemBelongingToUserId itemIdParam userId env
      A.Item.singleWire <$> justOr404 maybeItem

    updateItem itemIdParam itemUpdate = do
      env <- ask
      maybeItem <- C.Item.updateItem (A.Item.inputToItemUpdate itemIdParam userId itemUpdate) env
      A.Item.singleWire <$> justOr404 maybeItem
-- forbidden handler below is getting silly - there must be a better way
server _ = const forbidden :<|> const (forbidden :<|> forbidden :<|> const forbidden)
  where forbidden = throwError err403
