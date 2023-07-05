{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib (
  startApp
, app
) where

import Network.Wai
import Servant
import Types.Api
import Types.UserTag
import Env.Monad
import Network.Wai.Handler.Warp hiding (runEnv)
import Store.Tags
import Data.Maybe
import Control.Exception
import Data.Time
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.List (sortOn)
import Data.Ord (Down(..))

startApp :: IO ()
startApp = do
  store <- newTagStore
  let settings =
        setPort 8080 $
        setOnException apHandler
        defaultSettings
  runSettings settings $ app store

apHandler :: Maybe Request -> SomeException -> IO ()
apHandler _ = print

app :: TagStore -> Application
app store = serve (Proxy @API) $ hoistServer (Proxy @API) (`runEnv` store) server

server :: ServerT API Env
server = serverHealthcheck :<|> serverUserTags :<|> serverUserProfiles

serverHealthcheck :: Env RespHealthcheck
serverHealthcheck = pure $ RespHealthcheck "OK"

serverUserTags :: ReqUserTags -> Env NoContent
serverUserTags (ReqUserTags tag) = insertTag tag >> return NoContent

serverUserProfiles :: [String] -> TagTimeRange -> Maybe Int -> RespUserProfiles -> Env RespUserProfiles
serverUserProfiles segments timeRange limit'm expected = do
  cookie <- case segments of
    [segment] -> pure $ Cookie segment
    _ -> fail "Illegal number of segments"
  resp <- logicUserProfiles cookie timeRange limit'm
  when (resp /= expected) $ liftIO $ do
    putStrLn "Response:"
    print resp
    putStrLn "Expected:"
    print expected
  pure resp

logicUserProfiles :: Cookie -> TagTimeRange -> Maybe Int -> Env RespUserProfiles
logicUserProfiles cookie TagTimeRange{..} limit'm = do
  let beginInclusive' = TagTime $ localTimeToUTC utc beginInclusive
      endExclusive' = TagTime $ localTimeToUTC utc endExclusive

  let limit = fromMaybe 200 limit'm

  let filterTag UserTag{time} = beginInclusive' <= time && time < endExclusive'
      filterTags = take limit . filter filterTag

  let prepResults = sortOn (Down . time) . filterTags
  views <- prepResults <$> getTag VIEW cookie
  buys <- prepResults <$> getTag BUY cookie

  return RespUserProfiles{..}
