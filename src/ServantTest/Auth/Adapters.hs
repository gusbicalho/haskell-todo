module ServantTest.Auth.Adapters where

import Data.String (fromString)
import qualified Data.Text as T
import qualified ServantTest.Auth.WireTypes as Wire
import qualified ServantTest.Models.User as M.User

wireToLoginInput :: Wire.LoginInput -> M.User.LoginInput
wireToLoginInput (Wire.LoginInput username password) =
  M.User.LoginInput ( fromString $ T.unpack username )
                    ( fromString $ T.unpack password )

toIdentity :: M.User.User -> Wire.Identity
toIdentity M.User.User { M.User.userId } = Wire.User { Wire.userId }
