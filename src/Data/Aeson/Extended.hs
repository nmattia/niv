module Data.Aeson.Extended where

import Data.Aeson (ToJSON)
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as BL

--- Aeson

-- | Efficiently prettify and serialize a JSON value as a lazy 'BL.ByteString'
-- and write it to a file.
encodeFilePretty :: (ToJSON a) => FilePath -> a -> IO ()
encodeFilePretty fp = BL.writeFile fp . AesonPretty.encodePretty' config
  where
    config = AesonPretty.defConfig {
      AesonPretty.confTrailingNewline = True,
      AesonPretty.confCompare = compare
    }
