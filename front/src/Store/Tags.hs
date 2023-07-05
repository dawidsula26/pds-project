{-# LANGUAGE OverloadedStrings #-}
module Store.Tags (
  TagStore
, newTagStore
, insertTag
, getTag
) where

import qualified Data.Map.Strict as M
import Control.Monad.Reader
import Data.Has
import Types.UserTag
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:))
import Data.AesonBson
import Data.Aeson
import qualified Data.Text as T
import Data.List.Extra (enumerate)
import Control.Arrow
import Data.Maybe


data TagStore = TagStore {
  pipe :: Mongo.Pipe
, stores :: Action -> Store Cookie UserTag
}

newTagStore :: IO TagStore
newTagStore = do
  pipe <- Mongo.connect $ Mongo.Host "mongo-router" $ Mongo.PortNumber 27017
  let makeStore act = Store "tags" (T.toLower $ T.pack $ show act)
      actions = enumerate
      actionsWithStores = map (id &&& makeStore) actions
      storesMap = M.fromList actionsWithStores
      stores = (M.!) storesMap
  pure TagStore{..}

insertTag :: (MonadReader env m, Has TagStore env, MonadIO m) => UserTag -> m ()
insertTag tag@UserTag{cookie, action} = do
  TagStore{..} <- asks getter
  let store = stores action
  insertStore pipe store cookie tag

getTag :: (MonadReader env m, Has TagStore env, MonadIO m) => Action -> Cookie -> m [UserTag]
getTag action cookie = do
  TagStore{..} <- asks getter
  let store = stores action
  getStore pipe store cookie


data Store k v = Store {
  arrayLabel :: Mongo.Label
, collection :: Mongo.Collection
}

insertStore :: (MonadIO m, ToJSON k, ToJSON v) => Mongo.Pipe -> Store k v -> k -> v -> m ()
insertStore pipe Store{..} key value = writeToPipe pipe $ do
  let slice = ["$push" =: [arrayLabel =: [
          "$each" =: [toBSON value]
        , "$slice" =: -maxValuesPerKey
        ]]]
  Mongo.upsert (keySelection key collection) slice

getStore :: (MonadIO m, ToJSON k, FromJSON v) => Mongo.Pipe -> Store k v -> k -> m [v]
getStore pipe Store{..} key = readFromPipe pipe $ do
  document'm <- Mongo.findOne $ keySelection key collection
  let documents = case document'm of
        Nothing -> []
        Just doc -> fromMaybe (error $ "Expected " <> show arrayLabel <> " in document.") $
          Mongo.lookup arrayLabel doc
  pure $ map (fromBSON . Mongo.Doc) documents

keySelection :: (Mongo.Select aQueryOrSelection, ToJSON k) => k -> Mongo.Collection -> aQueryOrSelection
keySelection key = Mongo.select ["_id" =: toBSON key]

fromBSON :: FromJSON a => Mongo.Value -> a
fromBSON = \case {Success a -> a; Error e -> error e} . fromJSON . aesonifyValue

toBSON :: ToJSON a => a -> Mongo.Value
toBSON = bsonifyValue errorRange . toJSON

readFromPipe :: MonadIO m => Mongo.Pipe -> Mongo.Action m a -> m a
readFromPipe pipe = Mongo.access pipe Mongo.ReadStaleOk databaseName

writeToPipe :: MonadIO m => Mongo.Pipe -> Mongo.Action m a -> m a
writeToPipe pipe = Mongo.access pipe Mongo.UnconfirmedWrites databaseName

databaseName :: Mongo.Database
databaseName = "pds"

maxValuesPerKey :: Int
maxValuesPerKey = 10
