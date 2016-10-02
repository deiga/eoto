{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Images where

  import GHC.Generics
  import Data.Aeson.Types
  import Servant

  data Image = Image
    { imageId :: String
    , priority :: Int
    , profileId :: String
    , active :: Bool
    } deriving (Eq, Show, Generic)

  instance ToJSON Image

  type ImagesAPI = "images" :> Get '[JSON] [Image]

  imagesServer :: Server ImagesAPI
  imagesServer = getImages

    where getImages :: Handler [Image]
          getImages = return imagesSeed

  imagesSeed :: [Image]
  imagesSeed = []
