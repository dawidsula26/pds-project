module Env.Monad (
  Env (..)
, runEnv
) where

import Control.Monad.IO.Class
import Servant
import Control.Monad.Reader
import Store.Tags

newtype Env a = Env (ReaderT TagStore Handler a)
  deriving newtype (Functor, Applicative, Monad, MonadReader TagStore, MonadIO, MonadFail)

runEnv :: Env a -> TagStore -> Handler a
runEnv (Env m) = runReaderT m
