module Niv.Sources.Test where

import Niv.Sources
import Test.Tasty.HUnit ((@=?))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.Text as T

-- | Ensure that the sources.nix we ship is tracked as the latest version
test_shippedSourcesNixIsLatest :: IO ()
test_shippedSourcesNixIsLatest =
    latestVersionMD5 @=?
      (T.pack . show . MD5.md5 . BL.fromStrict $ initNixSourcesNixContent)
