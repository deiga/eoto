{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

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
  import Api.Images
  import Api.Users
  import Api.Credentials

  type CombinedAPI = UsersAPI
        :<|> ImagesAPI
        :<|> CredentialsAPI

  combinedServer :: Server CombinedAPI
  combinedServer = usersServer :<|> imagesServer :<|> credentialsServer


  api :: Proxy CombinedAPI
  api = Proxy

  app :: Application
  app = serve api combinedServer

  server :: IO ()
  server = run 8081 app
