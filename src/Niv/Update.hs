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
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.String
import UnliftIO
import qualified Control.Category as Cat
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HMS
-- import qualified Data.HashSet as HS
import qualified Data.Text as T

type Attrs = HMS.HashMap T.Text (Freedom, Value)

data Field = Field
  { name :: T.Text
  , short :: Char
  , metavar :: T.Text
  , help :: T.Text -> T.Text
  , hidden :: Bool
  }

instance IsString Field where
  fromString str = do
    let t = T.pack str
        mv = T.toUpper t
    Field
      { name = t
      , short = T.head t
      , metavar = mv
      , help = \mv' -> "Equivalent to --attribute " <> t <> "=" <> mv'
      , hidden = False
      }

data Update b c where
  Id :: Update a a
  Compose :: (Compose b c) -> Update b c
  Arr :: (b -> c) -> Update b c
  First :: Update b c -> Update (b, d) (c, d)
  Zero :: Update b c
  Plus :: Update b c -> Update b c -> Update b c
  Check :: (a -> Bool) -> Update (Box a) ()
  Load :: Field -> Update () (Box Value)
  UseOrSet :: Field -> Update (Box Value) (Box Value)
  Update :: Field -> Update (Box Value) (Box Value)
  Run :: (a -> IO b)  -> Update (Box a) (Box b)
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
    Compose (Compose' f g)-> "(" <> show f <> " . " <> show g <> ")"
    Arr _f -> "Arr"
    First a -> "First " <> show a
    Zero -> "Zero"
    Plus l r -> "(" <> show l <> " + " <> show r <> ")"
    Check _ch -> "Check"
    Load k -> "Load " <> T.unpack (name k)
    UseOrSet k -> "UseOrSet " <> T.unpack (name k)
    Update k -> "Update " <> T.unpack (name k)
    Run _act -> "Io"
    Template -> "Template"

data Compose a c = forall b. Compose' (Update b c) (Update a b)

-- | Run an 'Update' and return the new attributes and result.
runUpdate :: Attrs -> Update () a -> IO (Attrs, a)
runUpdate (boxAttrs -> attrs) a = runUpdate' attrs a >>= feed
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
      FailZero -> T.unlines
        [ "A dead end was reached during evaluation."
        , "This is a bug. Please create a ticket:"
        , "  https://github.com/nmattia/niv/issues/new"
        , "Thanks! I'll buy you a beer."
        ]
      FailCheck -> "A check failed during update"
      FailTemplate tpl keys -> T.unlines
        [ "Could not render template " <> tpl
        , "with keys: " <> T.intercalate ", " keys
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
  deriving Show

data UpdateRes a b
  = UpdateReady (UpdateReady b)
  | UpdateNeedMore (a -> IO (UpdateReady b))
  deriving Functor

data UpdateReady b
  = UpdateSuccess BoxedAttrs b
  | UpdateFailed UpdateFailed
  deriving Functor

runBox :: Box a -> IO a
runBox = boxOp

data Box a = Box
  { boxNew :: Bool
    -- ^ Whether the value is new or was retrieved (or derived) from old
    -- attributes
  , boxOp :: IO a
  }
  deriving Functor

instance Applicative Box where
  pure x = Box { boxNew = False, boxOp = pure x }
  f <*> v = Box
    { boxNew = (||) (boxNew f) (boxNew v)
    , boxOp = boxOp f <*> boxOp v
    }

instance Semigroup a => Semigroup (Box a) where
  (<>) = liftA2 (<>)

instance IsString (Box T.Text) where
  fromString str = Box { boxNew = False, boxOp = pure $ T.pack str }

type BoxedAttrs = HMS.HashMap T.Text (Freedom, Box Value)

unboxAttrs :: BoxedAttrs -> IO Attrs
unboxAttrs = traverse (\(fr, v) -> (fr,) <$> runBox v)

boxAttrs :: Attrs -> BoxedAttrs
boxAttrs = fmap (\(fr, v) -> (fr,
    case fr of
      -- TODO: explain why hacky
      Locked -> (pure v) { boxNew = True } -- XXX: somewhat hacky
      Free -> pure v
    ))

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
    Plus l r -> runUpdate' attrs l >>= \case
      UpdateReady (UpdateFailed{}) -> runUpdate' attrs r
      UpdateReady (UpdateSuccess f v) -> pure $ UpdateReady (UpdateSuccess f v)
      UpdateNeedMore next -> pure $ UpdateNeedMore $ \v -> next v >>= \case
        UpdateSuccess f res -> pure $ UpdateSuccess f res
        UpdateFailed {} -> runUpdate' attrs r >>= \case
          UpdateReady res -> pure res
          UpdateNeedMore next' -> next' v
    Load k -> pure $ UpdateReady $ do
      case HMS.lookup (name k) attrs of
        Just (_, v') -> UpdateSuccess attrs v'
        Nothing -> UpdateFailed $ FailNoSuchKey (name k)
    First a -> do
      runUpdate' attrs a >>= \case
        UpdateReady (UpdateFailed e) -> pure $ UpdateReady $ UpdateFailed e
        UpdateReady (UpdateSuccess fo ba) -> pure $ UpdateNeedMore $ \gtt -> do
          pure $ UpdateSuccess fo (ba, snd gtt)
        UpdateNeedMore next -> pure $ UpdateNeedMore $ \gtt -> do
          next (fst gtt) >>= \case
            UpdateFailed e -> pure $ UpdateFailed e
            UpdateSuccess f res -> do
              pure $ UpdateSuccess f (res, snd gtt)
    Run act -> pure (UpdateNeedMore $ \gtt -> do
      pure $ UpdateSuccess attrs $ Box (boxNew gtt) (act =<< runBox gtt))
    Check ch -> pure (UpdateNeedMore $ \gtt -> do
      v <- runBox gtt
      if ch v
      then pure $ UpdateSuccess attrs ()
      else pure $ UpdateFailed FailCheck)
    UseOrSet k -> pure $ case HMS.lookup (name k) attrs of
      Just (Locked, v) -> UpdateReady $ UpdateSuccess attrs v
      Just (Free, v) -> UpdateReady $ UpdateSuccess attrs v
      Nothing -> UpdateNeedMore $ \gtt -> do
        let attrs' = HMS.singleton (name k) (Locked, gtt) <> attrs
        pure $ UpdateSuccess attrs' gtt
    Update k -> pure $ case HMS.lookup (name k) attrs of
      Just (Locked, v) -> UpdateReady $ UpdateSuccess attrs v
      Just (Free, v) -> UpdateNeedMore $ \gtt -> do
        if (boxNew gtt)
        then do
          v' <- boxOp v
          gtt' <- boxOp gtt
          -- Here we compare the old and new values, flagging the new one as
          -- "boxNew" iff they differ.
          -- TODO: generalize this to all boxes
          let gtt'' = if v' /= gtt' then gtt { boxNew = True, boxOp = pure gtt' }
                else gtt { boxNew = False, boxOp = pure gtt' }
          pure $ UpdateSuccess (HMS.insert (name k) (Locked, gtt'') attrs) gtt''
        else do
          pure $ UpdateSuccess attrs v
      Nothing -> UpdateNeedMore $ \gtt -> do
        pure $ UpdateSuccess (HMS.insert (name k) (Locked, gtt) attrs) gtt
    Compose (Compose' f g) -> runUpdate' attrs g >>= \case
      UpdateReady (UpdateFailed e) -> pure $ UpdateReady $ UpdateFailed e
      UpdateReady (UpdateSuccess attrs' act) -> runUpdate' attrs' f >>= \case
        UpdateReady (UpdateFailed e) -> pure $ UpdateReady $ UpdateFailed e
        UpdateReady (UpdateSuccess attrs'' act') -> pure $ UpdateReady $ UpdateSuccess attrs'' act'
        UpdateNeedMore next -> UpdateReady <$> next act
      UpdateNeedMore next -> pure $ UpdateNeedMore $ \gtt -> do
        next gtt >>= \case
          UpdateFailed e -> pure $ UpdateFailed e
          UpdateSuccess attrs' act -> runUpdate' attrs' f >>= \case
            UpdateReady ready -> pure ready
            UpdateNeedMore next' -> next' act
    Template -> pure $ UpdateNeedMore $ \v -> do
      v' <- runBox v
      case renderTemplate
            (\k ->
              ((decodeBox $ "When rendering template " <> v') . snd) <$>
              HMS.lookup k attrs) v' of
        Nothing -> pure $ UpdateFailed $ FailTemplate v' (HMS.keys attrs)
        Just v'' -> pure $ UpdateSuccess attrs (v'' <* v) -- carries over v's newness

-- data ArgTy = Needed T.Text | Optional T.Text

-- updateKeys :: Update a b -> HS.HashSet T.Text
-- updateKeys x = HS.unions (case x of
    -- Id -> []
    -- Compose (Compose' u1 u2)-> [updateKeys u1, updateKeys u2]
    -- First u -> [updateKeys u]
    -- Arr _f -> []
    -- Zero -> []
    -- Plus u1 u2 -> [updateKeys u1, updateKeys u2]
    -- Check _f -> []
    -- Load t -> [HS.singleton t]
    -- UseOrSet t -> [HS.singleton t]
    -- Update t -> [HS.singleton t]
    -- Run _f -> []
    -- Template -> []
    -- )

decodeBox :: FromJSON a => T.Text -> Box Value -> Box a
decodeBox msg v = v { boxOp = boxOp v >>= decodeValue msg }

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
    -- XXX: isn't this redundant?
    _ -> Just $ pure T.empty

template :: Update (Box T.Text) (Box T.Text)
template = Template

check :: (a -> Bool) -> Update (Box a) ()
check = Check

load :: FromJSON a => Field -> Update () (Box a)
load k = Load k >>> arr (decodeBox $ "When loading key " <> name k)

-- TODO: should input really be Box?
useOrSet :: JSON a => Field -> Update (Box a) (Box a)
useOrSet k =
    arr (fmap Aeson.toJSON) >>>
    UseOrSet k >>>
    arr (decodeBox $ "When trying to use or set key " <> name k)

update :: JSON a => Field -> Update (Box a) (Box a)
update k =
    arr (fmap Aeson.toJSON) >>>
    Update k >>>
    arr (decodeBox $ "When updating key " <> name k)

run :: (a -> IO b) -> Update (Box a) (Box b)
run = Run

-- | Like 'run' but forces evaluation
run' :: (a -> IO b) -> Update (Box a) (Box b)
run' act = Run act >>> dirty

dirty :: Update (Box a) (Box a)
dirty = arr (\v -> v { boxNew = True })
