{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeApplications #-}
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
import Network.Wai.Logger
import Control.Exception

startApp :: IO ()
startApp = do
  store <- newTagStore
  withStdoutLogger $ \apLogger -> do
    let settings =
          setPort 8080 $
          setLogger apLogger $
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

serverUserProfiles :: [String] -> TagTimeRange -> Maybe Int -> Env RespUserProfiles
serverUserProfiles segments TagTimeRange{..} limit'm = do
  cookie <- case segments of
    [segment] -> pure $ Cookie segment
    _ -> fail "Illegal number of segments"
  let limit = fromMaybe 200 limit'm

  let filterTag UserTag{time} = beginInclusive <= time && time < endExclusive
      filterTags = take limit . filter filterTag

  views <- filterTags <$> getTag VIEW cookie
  buys <- filterTags <$> getTag BUY cookie

  return RespUserProfiles{..}
