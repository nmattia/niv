module Niv.Test (tests, test) where

import qualified Niv.Git.Test as Git
import Niv.GitHub.Test
import Niv.Sources.Test
import Niv.Update.Test
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty

test :: IO ()
test = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "niv"
    [ Tasty.testGroup
        "update"
        [ Tasty.testCase "simply runs" simplyRuns,
          Tasty.testCase "picks first" picksFirst,
          Tasty.testCase "loads" loads,
          Tasty.testCase "survives checks" survivesChecks,
          Tasty.testCase "isn't too eager" isNotTooEager,
          Tasty.testCase "dirty forces update" dirtyForcesUpdate,
          Tasty.testCase "should run when no changes" shouldNotRunWhenNoChanges,
          Tasty.testCase "templates expand" templatesExpand
        ],
      Tasty.testGroup
        "github"
        [ Tasty.testCase "inits properly" test_githubInitsProperly,
          Tasty.testCase "updates" test_githubUpdates,
          Tasty.testCase "updates once" test_githubUpdatesOnce,
          Tasty.testCase "doesn't override rev" test_githubDoesntOverrideRev,
          Tasty.testCase "falls back to URL" test_githubURLFallback
        ],
      Tasty.testGroup
        "sources.nix"
        [ Tasty.testCase "has latest version" test_shippedSourcesNixIsLatest
        ],
      Tasty.testGroup "git" Git.tests
    ]
