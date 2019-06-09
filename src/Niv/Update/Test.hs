{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Niv.Update.Test where

import Control.Arrow
import Control.Monad
import Niv.Update
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T

simplyRuns :: IO ()
simplyRuns =
    void $ runUpdate attrs $ proc () -> do
      returnA -< ()
  where
    attrs = HMS.empty

picksFirst :: IO ()
picksFirst = do
    v <- execUpdate HMS.empty $
      let
        l = proc () -> do returnA -< 2
        r = proc () -> do returnA -< 3
      in l <+> r
    unless (v == (2::Int)) (error "bad value")

loads :: IO ()
loads = do
    v <- execUpdate attrs $ load "foo"
    v' <- runBox v
    unless (v' == ("bar" :: T.Text)) (error "bad value")
  where
    attrs = HMS.singleton "foo" (Locked, "bar")

survivesChecks :: IO ()
survivesChecks = do
    v <- execUpdate attrs $ proc () -> do
      (sawLeft <+> sawRight) -< ()
      load "res" -< ()
    v' <- runBox v
    unless (v' == ("I saw right" :: T.Text)) (error "bad value")
  where
    attrs = HMS.singleton "val" (Locked, "right")
    sawLeft :: Update () ()
    sawLeft = proc () -> do
      val <- load "val" -< ()
      check (== "left") -< (val :: Box T.Text)
      useOrSet "res" -< "I saw left" :: Box T.Text
      returnA -< ()
    sawRight :: Update () ()
    sawRight = proc () -> do
      val <- load "val" -< ()
      check (== "right") -< (val :: Box T.Text)
      useOrSet "res" -< "I saw right" :: Box T.Text
      returnA -< ()

isNotTooEager :: IO ()
isNotTooEager = do
    let f = constBox () >>>
              run (const $ error "IO is too eager (f)") >>>
              useOrSet "foo"
    let f1 = proc () -> do
              run (const $ error "IO is too eager (f1)") -< pure ()
              useOrSet "foo" -< "foo"
    void $ (execUpdate attrs f :: IO (Box T.Text))
    void $ (execUpdate attrs f1 :: IO (Box T.Text))
  where
    attrs = HMS.singleton "foo" (Locked, "right")

dirtyForcesUpdate :: IO ()
dirtyForcesUpdate = do
    let f = constBox ("world" :: T.Text) >>> dirty >>> update "hello"
    attrs' <- evalUpdate attrs f
    unless ((snd <$> HMS.lookup "hello" attrs') == Just "world") $
      error $ "bad value for hello: " <> show attrs'
  where
    attrs = HMS.singleton "hello" (Free, "foo")

shouldNotRunWhenNoChanges :: IO ()
shouldNotRunWhenNoChanges = do
    let f = proc () -> do
          update "hello" -< ("world" :: Box T.Text)
          run (\() -> error "io shouldn't be run") -< pure ()
    attrs <- evalUpdate HMS.empty f
    unless ((snd <$> HMS.lookup "hello" attrs) == Just "world") $
      error $ "bad value for hello: " <> show attrs
    let f' = proc () -> do
          run (\() -> error "io shouldn't be run") -< pure ()
          update "hello" -< ("world" :: Box T.Text)
    attrs' <- evalUpdate HMS.empty f'
    unless ((snd <$> HMS.lookup "hello" attrs') == Just "world") $
      error $ "bad value for hello: " <> show attrs'
    v3 <- execUpdate
      (HMS.fromList [("hello", (Free, "world")), ("bar", (Free, "baz"))]) $
      proc () -> do
          v1 <- update "hello" -< "world"
          v2 <- run (\_ -> error "io shouldn't be run") -< (v1 :: Box T.Text)
          v3 <- update "bar" -< (v2 :: Box T.Text)
          returnA -< v3
    v3' <- runBox v3
    unless (v3' == "baz") $ error "bad value"

templatesExpand :: IO ()
templatesExpand = do
    v3 <- execUpdate attrs $ proc () -> template -< "<v1>-<v2>"
    v3' <- runBox v3
    unless (v3' == "hello-world") $ error "bad value"
  where
    attrs = HMS.fromList [("v1", (Free, "hello")), ("v2", (Free, "world"))]

constBox :: a -> Update () (Box a)
constBox a = arr (const (pure a))
