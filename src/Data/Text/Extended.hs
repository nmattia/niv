module Data.Text.Extended where

import System.Exit (exitFailure)
import qualified Data.Text as T
import qualified Data.Text.IO as T

tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- not quite the perfect place for this
abort :: T.Text -> IO a
abort msg = do
    T.putStrLn msg
    exitFailure
