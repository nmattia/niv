module Niv.Cmd where

import qualified Data.Text as T
import Niv.Sources
import Niv.Update

data Cmd = Cmd
  { -- | Important: if an object is returned, then it should be accepted by 'acceptsCmd'
    parseCmdShortcut :: T.Text -> Maybe (PackageName, PackageSpec),
    updateCmd :: Update () (),
    name :: T.Text,
    -- | Some notes to print
    extraLogs :: Attrs -> [T.Text],
    -- | Returns True if this Cmd knows how to handle the
    -- given PackageSpec
    acceptsCmd :: PackageSpec -> Bool
  }
