{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Extended where

import qualified Data.Text as T
import Niv.Logger
import System.Exit (exitFailure)
import UnliftIO

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

-- not quite the perfect place for this
abort :: (MonadIO io) => T.Text -> io a
abort msg = do
  tsay $ T.unwords [tbold $ tred "FATAL:", msg]
  liftIO exitFailure
