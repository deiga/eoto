{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Tutorial where

  import Prelude ()
  import Prelude.Compat

  import Control.Monad.Except
  import Control.Monad.Reader
  import Data.Aeson.Compat
  import Data.Aeson.Types
  import Data.Attoparsec.ByteString
  import Data.ByteString (ByteString)
  import Data.List
  import Data.Maybe
  import Data.String.Conversions
  import Data.Time.Calendar
  import GHC.Generics
  import Network.HTTP.Media ((//), (/:))
  import Network.Wai
  import Network.Wai.Handler.Warp
  import Servant
  import System.Directory
  import qualified Data.Aeson.Parser

  data User = User
    { name :: String
    , age :: Int
    , email :: String
    , registrationDate :: Day
    } deriving (Eq, Show, Generic)

  instance ToJSON User

  users1 :: [User]
  users1 = [ isaac, albert ]

  users2 :: [User]
  users2 = [ isaac, albert ]

  type UserAPI1 = "users" :> Get '[JSON] [User]
  type UserAPI2 = "users" :> Get '[JSON] [User]
                :<|> "isaac" :> Get '[JSON] User
                :<|> "albert" :> Get '[JSON] User

  type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
        :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
        :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

  data Position = Position
    { xCoord :: Int
    , yCoord :: Int
    } deriving Generic

  instance ToJSON Position

  newtype HelloMessage = HelloMessage { msg :: String }
    deriving Generic

  instance ToJSON HelloMessage

  data ClientInfo = ClientInfo
    { clientName :: String
    , clientEmail :: String
    , clientAge :: Int
    , clientInterestedIn :: [String]
    } deriving Generic

  instance FromJSON ClientInfo
  instance ToJSON ClientInfo

  data Email = Email
    { from :: String
    , to :: String
    , subject :: String
    , body :: String
    } deriving Generic

  instance ToJSON Email

  emailForClient :: ClientInfo -> Email
  emailForClient c = Email from' to' subject' body'

    where from'    = "great@company.com"
          to'      = clientEmail c
          subject' = "Hey " ++ clientName c ++ ", we miss you!"
          body'    = "Hi " ++ clientName c ++ ",\n\n"
                  ++ "Since you've recently turned " ++ show (clientAge c)
                  ++ ", have you checked out our latest "
                  ++ intercalate ", " (clientInterestedIn c)
                  ++ " products? Give us a visit!"

  server3 :: Server API
  server3 = position
        :<|> hello
        :<|> marketing


    where position :: Int -> Int -> Handler Position
          position x y = return (Position x y)

          hello :: Maybe String -> Handler HelloMessage
          hello mname = return . HelloMessage $ case mname of
            Nothing -> "Hello, anonymous coward"
            Just n  -> "Hello, " ++ n

          marketing :: ClientInfo -> Handler Email
          marketing clientInfo = return (emailForClient clientInfo)


  type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent

  newtype FileContent = FileContent
    { content :: String }
    deriving Generic

  instance ToJSON FileContent

  server5 :: Server IOAPI1
  server5 = do
    fileContent <- liftIO (readFile "myfile.txt")
    return (FileContent fileContent)

  failingHandler :: Handler ()
  failingHandler = throwError myerr

    where myerr :: ServantErr
          myerr = err503 { errBody = "Sorry dear user." }

  isaac :: User
  isaac =  User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

  albert :: User
  albert = User  "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

  server1 :: Server UserAPI1
  server1 = return users1

  server2 :: Server UserAPI2
  server2 = return users2
        :<|> return isaac
        :<|> return albert

  server6 :: Server IOAPI1
  server6 = do
    exists <- liftIO (doesFileExist "myfile.txt")
    if exists
      then liftIO (readFile "myfile.txt") >>= return . FileContent
      else throwError custom404Err

    where custom404Err = err404 { errBody = "myfile.txt just isn't there, please leave this server alone." }

  type MyHandler = Get '[JSON] (Headers '[Header "X-Ant-Int" Int] User)

  type StaticAPI = "static" :> Raw

  staticAPI :: Proxy StaticAPI
  staticAPI = Proxy

  server7 :: Server StaticAPI
  server7 = serveDirectory "static-files"

  app3 :: Application
  app3 = serve staticAPI server7

  myHandler :: Server MyHandler
  myHandler = return $ addHeader 1797 albert

  type UserAPI3 = -- view the user with given userid, in JSON
                  Capture "userid" Int :> Get '[JSON] User

              :<|> -- delete the user with given userid. empty response
                  Capture "userid" Int :> DeleteNoContent '[JSON] NoContent

  type UserAPI4 = Capture "userid" Int :>
    (   Get '[JSON] User
    :<|> DeleteNoContent '[JSON] NoContent
    )

  server8 :: Server UserAPI3
  server8 = getUser :<|> deleteUser

    where getUser :: Int -> Handler User
          getUser _userid = error "..."

          deleteUser :: Int -> Handler NoContent
          deleteUser _userid = error "..."

  server9 :: Server UserAPI4
  server9 userid = getUser userid :<|> deleteUser userid

    where getUser :: Int -> Handler User
          getUser = error "..."

          deleteUser :: Int -> Handler NoContent
          deleteUser = error "..."

  -- API for values of type 'a'
  -- indexed by values of type 'i'
  type APIFor a i =
          Get '[JSON] [a] -- list 'a's
    :<|>  ReqBody '[JSON] a :> PostNoContent '[JSON] NoContent -- add an 'a'
    :<|>  Capture "id" i :>
            ( Get '[JSON] a -- view an a' given its "identifier" of type 'i'
        :<|>  ReqBody '[JSON] a :> PutNoContent '[JSON] NoContent -- update an 'a'
        :<|>  DeleteNoContent '[JSON] NoContent -- delete an 'a'
            )

  serverFor :: Handler [a] -- handler for listing of 'a's
            -> (a -> Handler NoContent) -- handler for adding an 'a'
            -> (i -> Handler a) -- handler for viewing an 'a' given its identidier 'i'
            -> (i -> a -> Handler NoContent) -- updateing an 'a' with given id
            -> (i -> Handler NoContent) -- deleting an 'a' given its id
            -> Server (APIFor a i)
  serverFor getEntities newEntity viewEntity updateEntity deleteEntity = getEntities :<|> newEntity :<|> entityOperations

    where entityOperations id = viewEntity id :<|> updateEntity id :<|> deleteEntity id

  userAPI :: Proxy UserAPI2
  userAPI = Proxy

  api :: Proxy API
  api = Proxy

  api5 :: Proxy IOAPI1
  api5 = Proxy

  app1 :: Application
  app1 = serve api5 server6

  server :: IO ()
  server = run 8081 app3
