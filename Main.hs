-- TODO: qualified imports
-- TODO: format code

import Options.Applicative
import Control.Monad
import Data.Semigroup ((<>))
import System.Directory
import System.FilePath

fileFetchNix :: FilePath
fileFetchNix = "nix" </> "fetch.nix"

fileFetchNixContent :: String
fileFetchNixContent = unlines
  [


  ]

fileVersionsJson :: FilePath
fileVersionsJson = "nix" </> "versions.json"

fileVersionsJsonContent :: String
fileVersionsJsonContent = unlines
  [

  ]

cmdInit :: IO ()
cmdInit = do
    putStrLn "Creating directory nix (if it doesn't exist)"
    createDirectoryIfMissing True "nix"

    putStrLn $ "Creating file " <> fileFetchNix <> " (if it doesn't exist)"
    fileFetchNixExists <- doesFileExist fileFetchNix

    if fileFetchNixExists
    then do
      putStrLn $ "Not writing " <> fileFetchNix
      putStrLn "(file exists)"
    else do
      putStrLn $ "Writing " <> fileFetchNix
      writeFile fileFetchNix fileFetchNixContent

    putStrLn $ "Creating file " <> fileVersionsJson <> " (if it doesn't exist)"
    fileVersionsJsonExists <- doesFileExist fileVersionsJson

    if fileVersionsJsonExists
    then do
      putStrLn $ "Not writing " <> fileVersionsJson
      putStrLn "(file exists)"
    else do
      putStrLn $ "Writing " <> fileVersionsJson
      writeFile fileVersionsJson fileVersionsJsonContent

cmdAdd :: IO ()
cmdAdd = putStrLn "add"

cmdShow :: IO ()
cmdShow = putStrLn "show"

cmdUpdate :: IO ()
cmdUpdate = putStrLn "update"

opts :: Parser (IO ())
opts = subparser (
    command "init" (info (pure cmdInit) idm) <>
    command "add"  (info (pure cmdAdd) idm) <>
    command "show"  (info (pure cmdAdd) idm) <>
    command "update"  (info (pure cmdAdd) idm) )

main :: IO ()
main = join $ execParser (info opts idm)
