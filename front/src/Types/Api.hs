{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types.Api (
  API
, RespHealthcheck (..)
, ReqUserTags (..)
, RespUserProfiles (..)
) where

import Servant
import Types.UserTag
import Data.Aeson
import GHC.Generics

type API = APIHealthcheck :<|> APIUserTags :<|> APIUserProfiles


type APIHealthcheck =
     "healthcheck"
  :> Get '[JSON] RespHealthcheck

newtype RespHealthcheck = RespHealthcheck String
  deriving newtype (FromJSON, ToJSON)


type APIUserTags =
     "user_tags"
  :> ReqBody '[JSON] ReqUserTags
  :> PostNoContent

newtype ReqUserTags = ReqUserTags UserTag
  deriving newtype (Show, Eq, FromJSON, ToJSON)


type APIUserProfiles =
     "user_profiles"
  :> CaptureAll "segments" String
  :> QueryParam' '[Required] "time_range" TagTimeRange
  :> QueryParam "limit" Int
  :> ReqBody '[JSON] RespUserProfiles
  :> Post '[JSON] RespUserProfiles

data RespUserProfiles = RespUserProfiles {
    cookie :: Cookie
  , views :: [UserTag]
  , buys :: [UserTag]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
