{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}
module Server where

import Servant
import ApiType
import qualified Server.User as U

server :: Server API
server = U.server
