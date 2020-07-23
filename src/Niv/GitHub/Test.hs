{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Niv.GitHub.Test where

import Control.Monad
import Data.Bifunctor
import qualified Data.HashMap.Strict as HMS
import Data.IORef
import Niv.GitHub
import Niv.GitHub.API
import Niv.Update

test_githubInitsProperly :: IO ()
test_githubInitsProperly = do
  actualState <- evalUpdate initialState $ proc () ->
    githubUpdate prefetch latestRev ghRepo -< ()
  unless ((snd <$> actualState) == expectedState)
    $ error
    $ "State mismatch: " <> show actualState
  where
    prefetch _ _ = pure "some-sha"
    latestRev _ _ _ = pure "some-rev"
    ghRepo _ _ =
      pure
        GithubRepo
          { repoDescription = Just "some-descr",
            repoHomepage = Just "some-homepage",
            repoDefaultBranch = Just "master"
          }
    initialState =
      HMS.fromList
        [ ("owner", (Free, "nmattia")),
          ("repo", (Free, "niv"))
        ]
    expectedState =
      HMS.fromList
        [ ("owner", "nmattia"),
          ("repo", "niv"),
          ("homepage", "some-homepage"),
          ("description", "some-descr"),
          ("branch", "master"),
          ("url", "https://github.com/nmattia/niv/archive/some-rev.tar.gz"),
          ("rev", "some-rev"),
          ("sha256", "some-sha"),
          ("type", "tarball"),
          ("url_template", "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz")
        ]

test_githubUpdates :: IO ()
test_githubUpdates = do
  actualState <- evalUpdate initialState $ proc () ->
    githubUpdate prefetch latestRev ghRepo -< ()
  unless ((snd <$> actualState) == expectedState)
    $ error
    $ "State mismatch: " <> show actualState
  where
    prefetch _ _ = pure "new-sha"
    latestRev _ _ _ = pure "new-rev"
    ghRepo _ _ =
      pure
        GithubRepo
          { repoDescription = Just "some-descr",
            repoHomepage = Just "some-homepage",
            repoDefaultBranch = Just "master"
          }
    initialState =
      HMS.fromList
        [ ("owner", (Free, "nmattia")),
          ("repo", (Free, "niv")),
          ("homepage", (Free, "some-homepage")),
          ("description", (Free, "some-descr")),
          ("branch", (Free, "master")),
          ("url", (Free, "https://github.com/nmattia/niv/archive/some-rev.tar.gz")),
          ("rev", (Free, "some-rev")),
          ("sha256", (Free, "some-sha")),
          ("type", (Free, "tarball")),
          ("url_template", (Free, "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"))
        ]
    expectedState =
      HMS.fromList
        [ ("owner", "nmattia"),
          ("repo", "niv"),
          ("homepage", "some-homepage"),
          ("description", "some-descr"),
          ("branch", "master"),
          ("url", "https://github.com/nmattia/niv/archive/new-rev.tar.gz"),
          ("rev", "new-rev"),
          ("sha256", "new-sha"),
          ("type", "tarball"),
          ("url_template", "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz")
        ]

test_githubDoesntOverrideRev :: IO ()
test_githubDoesntOverrideRev = do
  actualState <- evalUpdate initialState $ proc () ->
    githubUpdate prefetch latestRev ghRepo -< ()
  unless ((snd <$> actualState) == expectedState)
    $ error
    $ "State mismatch: " <> show actualState
  where
    prefetch _ _ = pure "new-sha"
    latestRev _ _ _ = error "shouldn't fetch rev"
    ghRepo _ _ = error "shouldn't fetch repo"
    initialState =
      HMS.fromList
        [ ("owner", (Free, "nmattia")),
          ("repo", (Free, "niv")),
          ("homepage", (Free, "some-homepage")),
          ("description", (Free, "some-descr")),
          ("branch", (Free, "master")),
          ("url", (Free, "https://github.com/nmattia/niv/archive/some-rev.tar.gz")),
          ("rev", (Locked, "custom-rev")),
          ("sha256", (Free, "some-sha")),
          ("type", (Free, "tarball")),
          ("url_template", (Free, "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"))
        ]
    expectedState =
      HMS.fromList
        [ ("owner", "nmattia"),
          ("repo", "niv"),
          ("homepage", "some-homepage"),
          ("description", "some-descr"),
          ("branch", "master"),
          ("url", "https://github.com/nmattia/niv/archive/custom-rev.tar.gz"),
          ("rev", "custom-rev"),
          ("sha256", "new-sha"),
          ("type", "tarball"),
          ("url_template", "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz")
        ]

-- TODO: HMS diff for test output
test_githubURLFallback :: IO ()
test_githubURLFallback = do
  actualState <- evalUpdate initialState $ proc () ->
    githubUpdate prefetch latestRev ghRepo -< ()
  unless ((snd <$> actualState) == expectedState)
    $ error
    $ "State mismatch: " <> show actualState
  where
    prefetch _ _ = pure "some-sha"
    latestRev _ _ _ = error "shouldn't fetch rev"
    ghRepo _ _ = error "shouldn't fetch repo"
    initialState =
      HMS.fromList
        [ ("url_template", (Free, "https://foo.com/<baz>.tar.gz")),
          ("baz", (Free, "tarball"))
        ]
    expectedState =
      HMS.fromList
        [ ("url_template", "https://foo.com/<baz>.tar.gz"),
          ("baz", "tarball"),
          ("url", "https://foo.com/tarball.tar.gz"),
          ("sha256", "some-sha"),
          ("type", "tarball")
        ]

test_githubUpdatesOnce :: IO ()
test_githubUpdatesOnce = do
  ioref <- newIORef False
  tmpState <- evalUpdate initialState $ proc () ->
    githubUpdate (prefetch ioref) latestRev ghRepo -< ()
  unless ((snd <$> tmpState) == expectedState)
    $ error
    $ "State mismatch: " <> show tmpState
  -- Set everything free
  let tmpState' = HMS.map (first (\_ -> Free)) tmpState
  actualState <- evalUpdate tmpState' $ proc () ->
    githubUpdate (prefetch ioref) latestRev ghRepo -< ()
  unless ((snd <$> actualState) == expectedState)
    $ error
    $ "State mismatch: " <> show actualState
  where
    prefetch ioref _ _ = do
      readIORef ioref >>= \case
        False -> pure ()
        True -> error "Prefetch should be called once!"
      writeIORef ioref True
      pure "new-sha"
    latestRev _ _ _ = pure "new-rev"
    ghRepo _ _ =
      pure
        GithubRepo
          { repoDescription = Just "some-descr",
            repoHomepage = Just "some-homepage",
            repoDefaultBranch = Just "master"
          }
    initialState =
      HMS.fromList
        [ ("owner", (Free, "nmattia")),
          ("repo", (Free, "niv")),
          ("homepage", (Free, "some-homepage")),
          ("description", (Free, "some-descr")),
          ("branch", (Free, "master")),
          ("url", (Free, "https://github.com/nmattia/niv/archive/some-rev.tar.gz")),
          ("rev", (Free, "some-rev")),
          ("sha256", (Free, "some-sha")),
          ("type", (Free, "tarball")),
          ("url_template", (Free, "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"))
        ]
    expectedState =
      HMS.fromList
        [ ("owner", "nmattia"),
          ("repo", "niv"),
          ("homepage", "some-homepage"),
          ("description", "some-descr"),
          ("branch", "master"),
          ("url", "https://github.com/nmattia/niv/archive/new-rev.tar.gz"),
          ("rev", "new-rev"),
          ("sha256", "new-sha"),
          ("type", "tarball"),
          ("url_template", "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz")
        ]
