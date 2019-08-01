module ServantTest.HttpApi.Item where

import Control.Monad.Except
import Control.Monad.Reader
import Servant
import Servant.Auth.Server
import qualified Common.Auth.Types as AT
import qualified Common.Auth.Logic as Auth.Logic
import ServantTest.Env
import qualified ServantTest.WireTypes.Item as Wire.Item
import qualified ServantTest.Controllers.Item as C.Item
import qualified ServantTest.Adapters.Item as A.Item

type API = ReqBody '[JSON] Wire.Item.NewItemInput :> Post '[JSON] Wire.Item.SingleItem
      :<|> Capture "itemid" Integer :> (
                Get '[JSON] Wire.Item.SingleItem
           :<|> Delete '[JSON] Wire.Item.SingleItem
           :<|> ReqBody '[JSON] Wire.Item.ItemUpdateInput :> Put '[JSON] Wire.Item.SingleItem
           )

api :: Proxy API
api = Proxy

type ServerConstraints m = ( MonadError ServantErr m
                           , MonadIO m
                           , MonadReader Env m
                           )

justOr404 :: MonadError ServantErr m => Maybe a -> m a
justOr404 Nothing     = throwError err404
justOr404 (Just item) = return item

server :: ServerConstraints m => AuthResult AT.AuthTokenClaims -> ServerT API m
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
