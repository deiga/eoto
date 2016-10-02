{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Credentials where

  import GHC.Generics
  import Data.Aeson.Types
  import Servant

  data Credential = Credential
    { id :: String
    , username :: String
    , password :: String
    , token :: String
    , service :: String
    , userId :: String
    , active :: Bool
    } deriving (Eq, Show, Generic)

  instance ToJSON Credential

  type CredentialsAPI = "credentials" :> Get '[JSON] [Credential]

  credentialsServer :: Server CredentialsAPI
  credentialsServer = getCredentials

    where getCredentials :: Handler [Credential]
          getCredentials = return credentialsSeed

  credentialsSeed :: [Credential]
  credentialsSeed = []
