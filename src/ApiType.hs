module ApiType where

import Servant.API
import ApiType.User (UserAPI)

type API = UserAPI
