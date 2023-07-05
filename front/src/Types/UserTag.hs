{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.UserTag (
  UserTag (..)
, TagTime (..)
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

, TagTimeRange (..)
) where

import Data.Time
import Data.Aeson
import GHC.Generics
import Data.Char (toUpper)
import Servant
import qualified Data.Text as T


data UserTag = UserTag {
  time :: TagTime
, cookie :: Cookie
, country :: Country
, device :: Device
, action :: Action
, origin :: Origin
, product_info :: ProductInfo
} deriving (Show, Eq, Generic, FromJSON, ToJSON)


newtype TagTime = TagTime UTCTime
  deriving newtype (Show, Eq, Ord, FromJSON, ToJSON, FromHttpApiData)


newtype Cookie = Cookie String
  deriving newtype (Show, Eq, Ord, FromJSON, ToJSON)


newtype Country = Country String
  deriving newtype (Show, Eq, FromJSON, ToJSON)


data Device = PC | MOBILE | TV
  deriving (Show, Eq, Generic, Bounded, Enum)

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
  deriving newtype (Show, Eq, FromJSON, ToJSON)


data ProductInfo = ProductInfo {
  product_id :: ProductId
, brand_id :: BrandId
, category_id :: CategoryId
, price :: Price
} deriving (Show, Eq, Generic, FromJSON, ToJSON)


newtype ProductId = ProductId Int
  deriving newtype (Show, Eq, FromJSON, ToJSON)

newtype BrandId = BrandId String
  deriving newtype (Show, Eq, FromJSON, ToJSON)

newtype CategoryId = CategoryId String
  deriving newtype (Show, Eq, FromJSON, ToJSON)

newtype Price = Price Int
  deriving newtype (Show, Eq, FromJSON, ToJSON)


uppercaseSumTypeOptions :: Options
uppercaseSumTypeOptions = defaultOptions {
    constructorTagModifier = map toUpper
  }


data TagTimeRange = TagTimeRange {
  beginInclusive :: LocalTime
, endExclusive :: LocalTime
} deriving (Show)

instance FromHttpApiData TagTimeRange where
  parseUrlPiece txt = case T.splitOn "_" txt of
    [beginInclusive', endExclusive'] -> do
      beginInclusive <- parseUrlPiece beginInclusive'
      endExclusive <- parseUrlPiece endExclusive'
      pure TagTimeRange{..}
    _ -> throwError "Illegal number of \"_\" in input."
