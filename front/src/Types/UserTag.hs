{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Types.UserTag (
  UserTag (..)
, TagTime
, Cookie (..)
, Country
, Device
, Action (..)
, Origin

, ProductInfo (..)
, ProductId
, BrandId
, CategoryId
, Price
) where

import Data.Time
import Data.Aeson
import GHC.Generics
import Data.Char (toUpper)


data UserTag = UserTag {
  time :: TagTime
, cookie :: Cookie
, country :: Country
, device :: Device
, action :: Action
, origin :: Origin
, product_info :: ProductInfo
} deriving (Show, Generic, FromJSON, ToJSON)


newtype TagTime = TagTime UTCTime
  deriving newtype (Show, FromJSON, ToJSON)


newtype Cookie = Cookie String
  deriving newtype (Show, Eq, Ord, FromJSON, ToJSON)


newtype Country = Country String
  deriving newtype (Show, FromJSON, ToJSON)


data Device = PC | MOBILE | TV
  deriving (Show, Generic, Bounded, Enum)

instance FromJSON Device where
  parseJSON = genericParseJSON uppercaseSumTypeOptions

instance ToJSON Device where
  toJSON = genericToJSON uppercaseSumTypeOptions


data Action = VIEW | BUY
  deriving (Show, Eq, Ord, Generic, Bounded, Enum)

instance FromJSON Action where
  parseJSON = genericParseJSON uppercaseSumTypeOptions

instance ToJSON Action where
  toJSON = genericToJSON uppercaseSumTypeOptions


newtype Origin = Origin String
  deriving newtype (Show, FromJSON, ToJSON)


data ProductInfo = ProductInfo {
  product_id :: ProductId
, brand_id :: BrandId
, category_id :: CategoryId
, price :: Price
} deriving (Show, Generic, FromJSON, ToJSON)


newtype ProductId = ProductId String
  deriving newtype (Show, FromJSON, ToJSON)

newtype BrandId = BrandId String
  deriving newtype (Show, FromJSON, ToJSON)

newtype CategoryId = CategoryId String
  deriving newtype (Show, FromJSON, ToJSON)

newtype Price = Price Int
  deriving newtype (Show, FromJSON, ToJSON)


uppercaseSumTypeOptions :: Options
uppercaseSumTypeOptions = defaultOptions {
    constructorTagModifier = map toUpper
  }
