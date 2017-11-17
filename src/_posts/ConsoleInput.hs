{-# LANGUAGE OverloadedStrings #-}
-- | Utilities for command line prompts and confirmations.
module ConsoleInput where

import           Control.Monad.IO.Class
import           Data.Semigroup
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T

prompt :: MonadIO m => Text -> m Text
prompt t = liftIO $ do
  T.putStrLn t
  T.getLine

confirm :: MonadIO m => Text -> m Bool
confirm t = liftIO $ do
  l <- prompt (t <> " (y/N)")
  return (T.strip l == "y")
