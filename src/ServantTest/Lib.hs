module ServantTest.Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import ServantTest.HttpApi.ApiType (API)
import ServantTest.HttpApi.Server (server)

startApp :: IO ()
startApp = do
  let port = 8080
  putStrLn $ "Server running at port " ++ show port
  run port app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy
