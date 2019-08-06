module HaskellTodo.HttpApi.Static where

import Servant

type API = Raw

api :: Proxy API
api = Proxy

server :: ServerT API m
server = serveDirectoryWebApp "static"
