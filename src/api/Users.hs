{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Users where

  import GHC.Generics
  import Data.Aeson.Types
  import Servant

  data User = User
    { userId :: String
    , active :: Bool
    } deriving (Eq, Show, Generic)

  instance ToJSON User

  type UsersAPI = "users" :> Get '[JSON] [User]
            :<|>  "users" :> "active" :> Get '[JSON] [User]

  usersServer :: Server UsersAPI
  usersServer = getUsers :<|> getActiveUsers

    where getUsers :: Handler [User]
          getUsers = return usersSeed

          getActiveUsers :: Handler [User]
          getActiveUsers = return (filter active usersSeed)

  usersSeed :: [User]
  usersSeed = [ User "oFw+y12VVMefJFK2z84H0Sdk6sQV1GG2y6ffT0pm3A8=" True
          , User "SbQ3fz3NNQLei81aN4j5WcOmluVJkdT82ABK15S3cqk=" True
          , User "GFLFXLdpicEm9xxMrLT6Df/lSoJzPb+v0KQ9pT9ecxc=" False
          ]
