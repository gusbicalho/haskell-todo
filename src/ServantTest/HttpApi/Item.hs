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

type API = Capture "itemid" Integer :> Get '[JSON] Wire.Item.SingleItem
      :<|> ReqBody '[JSON] Wire.Item.NewItemInput :> Post '[JSON] Wire.Item.SingleItem

api :: Proxy API
api = Proxy

type ServerConstraints m = ( MonadError ServantErr m
                           , MonadIO m
                           , MonadReader Env m
                           )



server :: ServerConstraints m => AuthResult AT.AuthTokenClaims -> ServerT API m
server (Auth.Logic.authenticatedUserId -> Just userId) =
         getItem
    :<|> createItem
  where -- handlers
    getItem itemIdParam = do
      env <- ask
      maybeItem <- C.Item.getItemBelongingToUserId itemIdParam userId env
      case maybeItem of
        Nothing -> throwError err404
        Just item -> return $ A.Item.singleWire item

    createItem newItemInput
      | Wire.Item.input_userId newItemInput /= userId = throwError err403
      | otherwise = do
          env <- ask
          item <- C.Item.createItem (A.Item.inputToNewItem newItemInput) env
          return $ A.Item.singleWire item

server _ = (const $ throwError err403) :<|> (const $ throwError err403)
