module HaskellTodo.Controllers.User where

import Data.Text as T
import Common.HasField
import Common.Db.Transactor (Transactor(..))
import Common.Crypto.Hasher (Hasher(..))
import HaskellTodo.Models.User
import HaskellTodo.Db.User as Db.User

type ControllerConstraints env m transactor dbAction hasher =
  ( HasField "transactor" env transactor, Transactor transactor m dbAction, UserDb dbAction
  , HasField "hasher" env hasher, Hasher hasher m T.Text T.Text
  )

listUsers :: ControllerConstraints env m transactor dbAction hasher => env -> m [User]
listUsers env = do
  let transactor = #transactor env
  transact transactor Db.User.listUsers

getUser :: ControllerConstraints env m transactor dbAction hasher => Integer -> env -> m (Maybe User)
getUser idParam env = do
  let transactor = #transactor env
  transact transactor $ Db.User.getUser idParam

createUser :: ControllerConstraints env m transactor dbAction hasher => NewPlainUser -> env -> m (Maybe User)
createUser newPlainUser env = do
  let transactor = #transactor env
      hasher = #hasher env
  maybeNewUser <- withHashedPassword hasher newPlainUser
  case maybeNewUser of
    Nothing -> return Nothing
    Just newUser -> Just <$> transact transactor (Db.User.createUser newUser)

checkLogin :: ControllerConstraints env m transactor dbAction hasher => LoginInput -> env -> m (Maybe User)
checkLogin (LoginInput login plainPassword) env = do
  let transactor = #transactor env
      hasher = #hasher env
  maybeUser <- transact transactor $ Db.User.findUserByLogin login
  case maybeUser of
    Nothing -> return Nothing
    Just user -> do
      passwordMatches <- validatePassword hasher (userPassword user) plainPassword
      return $ if passwordMatches then Just user else Nothing

withHashedPassword :: Hasher hasher m T.Text T.Text => hasher -> NewPlainUser -> m (Maybe NewUser)
withHashedPassword hasher (NewPlainUser username plainPassword) = do
  maybePassword <- maybeHashPassword hasher plainPassword
  return $ NewUser username <$> maybePassword

maybeHashPassword :: Hasher hasher m T.Text T.Text => hasher -> PlainPassword -> m (Maybe Password)
maybeHashPassword hasher plain = do
  maybeHashed <- maybeHash hasher (plainPasswordToText plain)
  return $ textToPassword <$> maybeHashed

validatePassword :: Hasher hasher m T.Text T.Text => hasher -> Password -> PlainPassword -> m Bool
validatePassword hasher password plain = validate hasher (passwordToText password) (plainPasswordToText plain)
