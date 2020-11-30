module Data.HashMap.Strict.Extended where

import Control.Monad
import qualified Data.HashMap.Strict as HMS
import Data.Hashable (Hashable)

--- HashMap

forWithKeyM ::
  (Eq k, Hashable k, Monad m) =>
  HMS.HashMap k v1 ->
  (k -> v1 -> m v2) ->
  m (HMS.HashMap k v2)
forWithKeyM = flip mapWithKeyM

forWithKeyM_ ::
  (Eq k, Hashable k, Monad m) =>
  HMS.HashMap k v1 ->
  (k -> v1 -> m ()) ->
  m ()
forWithKeyM_ = flip mapWithKeyM_

mapWithKeyM ::
  (Eq k, Hashable k, Monad m) =>
  (k -> v1 -> m v2) ->
  HMS.HashMap k v1 ->
  m (HMS.HashMap k v2)
mapWithKeyM f m = do
  fmap mconcat $
    forM (HMS.toList m) $ \(k, v) ->
      HMS.singleton k <$> f k v

mapWithKeyM_ ::
  (Eq k, Hashable k, Monad m) =>
  (k -> v1 -> m ()) ->
  HMS.HashMap k v1 ->
  m ()
mapWithKeyM_ f m = do
  forM_ (HMS.toList m) $ \(k, v) ->
    HMS.singleton k <$> f k v
