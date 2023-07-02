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
import Network.Wai.Handler.Warp (run)
import Store.Tags

startApp :: IO ()
startApp = do
  store <- newTagStore
  run 8080 $ app store

app :: TagStore -> Application
app store = serve (Proxy @API) $ hoistServer (Proxy @API) (`runEnv` store) server

server :: ServerT API Env
server = serverHealthcheck :<|> serverUserTags :<|> serverUserProfiles

serverHealthcheck :: Env RespHealthcheck
serverHealthcheck = pure $ RespHealthcheck "OK"

serverUserTags :: ReqUserTags -> Env NoContent
serverUserTags (ReqUserTags tag) = insertTag tag >> return NoContent

serverUserProfiles :: [String] -> String -> Maybe Int -> Env RespUserProfiles
serverUserProfiles segments _ _ = do
  cookie <- case segments of
    [segment] -> pure $ Cookie segment
    _ -> fail "Illegal number of segments"
  views <- getTag VIEW cookie
  buys <- getTag BUY cookie
  return RespUserProfiles{..}
