{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Niv.Update where

import Control.Applicative
import Control.Arrow
import qualified Control.Category as Cat
import Data.Aeson (FromJSON, ToJSON, Value)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HMS
import Data.String
import qualified Data.Text as T
import Niv.Logger
import UnliftIO

type Attrs = HMS.HashMap T.Text (Freedom, Value)

data Update b c where
  Id :: Update a a
  Compose :: (Compose b c) -> Update b c
  Arr :: (b -> c) -> Update b c
  First :: Update b c -> Update (b, d) (c, d)
  Zero :: Update b c
  Plus :: Update b c -> Update b c -> Update b c
  Check :: (a -> Bool) -> Update (Box a) ()
  Load :: T.Text -> Update () (Box Value)
  UseOrSet :: T.Text -> Update (Box Value) (Box Value)
  Update :: T.Text -> Update (Box Value) (Box Value)
  Run :: (a -> IO b) -> Update (Box a) (Box b)
  Template :: Update (Box T.Text) (Box T.Text)

instance ArrowZero Update where
  zeroArrow = Zero

instance ArrowPlus Update where
  (<+>) = Plus

instance Arrow Update where
  arr = Arr
  first = First

instance Cat.Category Update where
  id = Id
  f . g = Compose (Compose' f g)

instance Show (Update b c) where
  show = \case
    Id -> "Id"
    Compose (Compose' f g) -> "(" <> show f <> " . " <> show g <> ")"
    Arr _f -> "Arr"
    First a -> "First " <> show a
    Zero -> "Zero"
    Plus l r -> "(" <> show l <> " + " <> show r <> ")"
    Check _ch -> "Check"
    Load k -> "Load " <> T.unpack k
    UseOrSet k -> "UseOrSet " <> T.unpack k
    Update k -> "Update " <> T.unpack k
    Run _act -> "Io"
    Template -> "Template"

data Compose a c = forall b. Compose' (Update b c) (Update a b)

-- | Run an 'Update' and return the new attributes and result.
runUpdate :: Attrs -> Update () a -> IO (Attrs, a)
runUpdate (attrs) a = boxAttrs attrs >>= flip runUpdate' a >>= feed
  where
    feed = \case
      UpdateReady res -> hndl res
      UpdateNeedMore next -> next (()) >>= hndl
    hndl = \case
      UpdateSuccess f v -> (,v) <$> unboxAttrs f
      UpdateFailed e -> error $ "Update failed: " <> T.unpack (prettyFail e)
    prettyFail :: UpdateFailed -> T.Text
    prettyFail = \case
      FailNoSuchKey k -> "Key could not be found: " <> k
      FailZero -> bug "A dead end was reached during evaluation."
      FailCheck -> "A check failed during update"
      FailTemplate tpl keys ->
        T.unlines
          [ "Could not render template " <> tpl,
            "with keys: " <> T.intercalate ", " keys
          ]

execUpdate :: Attrs -> Update () a -> IO a
execUpdate attrs a = snd <$> runUpdate attrs a

evalUpdate :: Attrs -> Update () a -> IO Attrs
evalUpdate attrs a = fst <$> runUpdate attrs a

tryEvalUpdate :: Attrs -> Update () a -> IO (Either SomeException Attrs)
tryEvalUpdate attrs upd = tryAny (evalUpdate attrs upd)

type JSON a = (ToJSON a, FromJSON a)

data UpdateFailed
  = FailNoSuchKey T.Text
  | FailZero
  | FailCheck
  | FailTemplate T.Text [T.Text]
  deriving (Show)

data UpdateRes a b
  = UpdateReady (UpdateReady b)
  | UpdateNeedMore (a -> IO (UpdateReady b))
  deriving (Functor)

data UpdateReady b
  = UpdateSuccess BoxedAttrs b
  | UpdateFailed UpdateFailed
  deriving (Functor)

runBox :: Box a -> IO a
runBox = boxOp

data Box a = Box
  { -- | Whether the value is new or was retrieved (or derived) from old
    -- attributes
    boxNew :: Bool,
    boxOp :: IO a
  }
  deriving (Functor)

mkBox :: Box a -> IO (Box a)
mkBox b = do
  mvar <- newMVar Nothing
  pure b {boxOp = singleton mvar (boxOp b)}

singleton :: MVar (Maybe a) -> IO a -> IO a
singleton mvar def = do
  modifyMVar mvar $ \case
    Just a -> pure (Just a, a)
    Nothing -> do
      a <- def
      pure (Just a, a)

instance Applicative Box where
  pure x = Box {boxNew = False, boxOp = pure x}
  f <*> v =
    Box
      { boxNew = (||) (boxNew f) (boxNew v),
        boxOp = boxOp f <*> boxOp v
      }

instance Semigroup a => Semigroup (Box a) where
  (<>) = liftA2 (<>)

instance IsString (Box T.Text) where
  fromString str = Box {boxNew = False, boxOp = pure $ T.pack str}

type BoxedAttrs = HMS.HashMap T.Text (Freedom, Box Value)

unboxAttrs :: BoxedAttrs -> IO Attrs
unboxAttrs = traverse (\(fr, v) -> (fr,) <$> runBox v)

boxAttrs :: Attrs -> IO BoxedAttrs
boxAttrs =
  mapM
    ( \(fr, v) -> do
        box <- mkBox (pure v)
        pure
          ( fr,
            case fr of
              -- TODO: explain why hacky
              Locked -> box {boxNew = True} -- XXX: somewhat hacky
              Free -> box
          )
    )

data Freedom
  = Locked
  | Free
  deriving (Eq, Show)

-- | Runs an update, trying to evaluate the 'Box'es as little as possible.
-- This is a hairy piece of code, apologies ¯\_(ツ)_/¯
-- In most cases I just picked the first implementation that compiled
runUpdate' :: BoxedAttrs -> Update a b -> IO (UpdateRes a b)
runUpdate' attrs = \case
  Id -> pure $ UpdateNeedMore $ pure . UpdateSuccess attrs
  Arr f -> pure $ UpdateNeedMore $ pure . UpdateSuccess attrs . f
  Zero -> pure $ UpdateReady (UpdateFailed FailZero)
  Plus l r ->
    runUpdate' attrs l >>= \case
      UpdateReady (UpdateFailed {}) -> runUpdate' attrs r
      UpdateReady (UpdateSuccess f v) -> pure $ UpdateReady (UpdateSuccess f v)
      UpdateNeedMore next -> pure $
        UpdateNeedMore $ \v ->
          next v >>= \case
            UpdateSuccess f res -> pure $ UpdateSuccess f res
            UpdateFailed {} ->
              runUpdate' attrs r >>= \case
                UpdateReady res -> pure res
                UpdateNeedMore next' -> next' v
  Load k -> pure $
    UpdateReady $ do
      case HMS.lookup k attrs of
        Just (_, v') -> UpdateSuccess attrs v'
        Nothing -> UpdateFailed $ FailNoSuchKey k
  First a -> do
    runUpdate' attrs a >>= \case
      UpdateReady (UpdateFailed e) -> pure $ UpdateReady $ UpdateFailed e
      UpdateReady (UpdateSuccess fo ba) -> pure $
        UpdateNeedMore $ \gtt -> do
          pure $ UpdateSuccess fo (ba, snd gtt)
      UpdateNeedMore next -> pure $
        UpdateNeedMore $ \gtt -> do
          next (fst gtt) >>= \case
            UpdateFailed e -> pure $ UpdateFailed e
            UpdateSuccess f res -> do
              pure $ UpdateSuccess f (res, snd gtt)
  Run act ->
    pure
      ( UpdateNeedMore $ \gtt -> do
          box <- mkBox $ Box (boxNew gtt) (act =<< runBox gtt)
          pure $ UpdateSuccess attrs box
      )
  Check ch ->
    pure
      ( UpdateNeedMore $ \gtt -> do
          v <- runBox gtt
          if ch v
            then pure $ UpdateSuccess attrs ()
            else pure $ UpdateFailed FailCheck
      )
  UseOrSet k -> pure $ case HMS.lookup k attrs of
    Just (Locked, v) -> UpdateReady $ UpdateSuccess attrs v
    Just (Free, v) -> UpdateReady $ UpdateSuccess attrs v
    Nothing -> UpdateNeedMore $ \gtt -> do
      let attrs' = HMS.singleton k (Locked, gtt) <> attrs
      pure $ UpdateSuccess attrs' gtt
  Update k -> pure $ case HMS.lookup k attrs of
    Just (Locked, v) -> UpdateReady $ UpdateSuccess attrs v
    Just (Free, v) -> UpdateNeedMore $ \gtt -> do
      if (boxNew gtt)
        then do
          v' <- boxOp v
          gtt' <- boxOp gtt
          -- Here we compare the old and new values, flagging the new one as
          -- "boxNew" iff they differ.
          -- TODO: generalize this to all boxes
          let gtt'' =
                if v' /= gtt'
                  then gtt {boxNew = True, boxOp = pure gtt'}
                  else gtt {boxNew = False, boxOp = pure gtt'}
          pure $ UpdateSuccess (HMS.insert k (Locked, gtt'') attrs) gtt''
        else do
          pure $ UpdateSuccess attrs v
    Nothing -> UpdateNeedMore $ \gtt -> do
      pure $ UpdateSuccess (HMS.insert k (Locked, gtt) attrs) gtt
  Compose (Compose' f g) ->
    runUpdate' attrs g >>= \case
      UpdateReady (UpdateFailed e) -> pure $ UpdateReady $ UpdateFailed e
      UpdateReady (UpdateSuccess attrs' act) ->
        runUpdate' attrs' f >>= \case
          UpdateReady (UpdateFailed e) -> pure $ UpdateReady $ UpdateFailed e
          UpdateReady (UpdateSuccess attrs'' act') -> pure $ UpdateReady $ UpdateSuccess attrs'' act'
          UpdateNeedMore next -> UpdateReady <$> next act
      UpdateNeedMore next -> pure $
        UpdateNeedMore $ \gtt -> do
          next gtt >>= \case
            UpdateFailed e -> pure $ UpdateFailed e
            UpdateSuccess attrs' act ->
              runUpdate' attrs' f >>= \case
                UpdateReady ready -> pure ready
                UpdateNeedMore next' -> next' act
  Template -> pure $
    UpdateNeedMore $ \v -> do
      v' <- runBox v
      case renderTemplate
        ( \k ->
            ((decodeBox $ "When rendering template " <> v') . snd)
              <$> HMS.lookup k attrs
        )
        v' of
        Nothing -> pure $ UpdateFailed $ FailTemplate v' (HMS.keys attrs)
        Just v'' -> pure $ UpdateSuccess attrs (v'' <* v) -- carries over v's newness

decodeBox :: FromJSON a => T.Text -> Box Value -> Box a
decodeBox msg v = v {boxOp = boxOp v >>= decodeValue msg}

decodeValue :: FromJSON a => T.Text -> Value -> IO a
decodeValue msg v = case Aeson.fromJSON v of
  Aeson.Success x -> pure x
  Aeson.Error str ->
    error $ T.unpack msg <> ": Could not decode: " <> show v <> ": " <> str

-- | Renders the template. Returns 'Nothing' if some of the attributes are
-- missing.
--  renderTemplate ("foo" -> "bar") "<foo>" -> pure (Just "bar")
--  renderTemplate ("foo" -> "bar") "<baz>" -> pure Nothing
renderTemplate :: (T.Text -> Maybe (Box T.Text)) -> T.Text -> Maybe (Box T.Text)
renderTemplate vals = \case
  (T.uncons -> Just ('<', str)) -> do
    case T.span (/= '>') str of
      (key, T.uncons -> Just ('>', rest)) -> do
        let v = vals key
        (liftA2 (<>) v) (renderTemplate vals rest)
      _ -> Nothing
  (T.uncons -> Just (c, str)) -> fmap (T.cons c) <$> renderTemplate vals str
  (T.uncons -> Nothing) -> Just $ pure T.empty

template :: Update (Box T.Text) (Box T.Text)
template = Template

check :: (a -> Bool) -> Update (Box a) ()
check = Check

load :: FromJSON a => T.Text -> Update () (Box a)
load k = Load k >>> arr (decodeBox $ "When loading key " <> k)

-- TODO: should input really be Box?
useOrSet :: JSON a => T.Text -> Update (Box a) (Box a)
useOrSet k =
  arr (fmap Aeson.toJSON)
    >>> UseOrSet k
    >>> arr (decodeBox $ "When trying to use or set key " <> k)

update :: JSON a => T.Text -> Update (Box a) (Box a)
update k =
  arr (fmap Aeson.toJSON)
    >>> Update k
    >>> arr (decodeBox $ "When updating key " <> k)

run :: (a -> IO b) -> Update (Box a) (Box b)
run = Run

-- | Like 'run' but forces evaluation
run' :: (a -> IO b) -> Update (Box a) (Box b)
run' act = Run act >>> dirty

dirty :: Update (Box a) (Box a)
dirty = arr (\v -> v {boxNew = True})
