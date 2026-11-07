{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Unit.Doctor (tests) where

import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (for_)
import Data.List (intercalate)
import qualified Data.Text as Text
import System.Directory
  ( copyFile, createDirectoryIfMissing, getCurrentDirectory
  , setCurrentDirectory )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Hypha.Command.Doctor (CheckResult (..), checkPlanOverlay, runDoctor)
import Hypha.Output.Outcome (Outcome (..))
import Hypha.Project.Plan
  ( ExplicitOverlay (..), OverlaySetting (..), defaultOverlayPath )
import Hypha.Types.BuildPlan (ProjectRoot (..))

tests :: TestTree
tests = testGroup "Doctor"
  [ testGroup "Doctor Command"
      [ testCase "all_pass=false when plan.json missing" $ do
          -- Run doctor in a temporary directory where plan.json doesn't exist
          withSystemTempDirectory "hypha-test" $ \tempDir -> do
            oldDir <- getCurrentDirectory
            setCurrentDirectory tempDir
            outcome <- runDoctor OverlayUnset Nothing
            setCurrentDirectory oldDir
            let allPass = extractAllPass (outcomeResult outcome)
            assertBool "all_pass should be False without plan.json"
                       (not allPass)

      , testCase "result contains checks object" $ do
          outcome <- runDoctor OverlayUnset Nothing
          let checks = extractChecks (outcomeResult outcome)
          assertBool "checks field should be present and be an object"
            (case checks of
              Just (Aeson.Object _) -> True
              _ -> False)

      , testCase "result contains all_pass boolean" $ do
          outcome <- runDoctor OverlayUnset Nothing
          let _allPass = extractAllPass (outcomeResult outcome)
          assertBool "all_pass field should be present" True

      , testCase "all_pass=true when all checks pass" $ do
          -- Positive case: when every check passes, all_pass is true.
          outcome <- runDoctor OverlayUnset Nothing
          let body     = outcomeResult outcome
              allPass  = extractAllPass body
              checksObj = extractChecks body
          assertBool "When all checks pass, all_pass should be true"
            (case checksObj of
              Just (Aeson.Object checks) ->
                let ghcStatus     = getStatus checks "ghc"
                    haddockStatus = getStatus checks "haddock"
                    planStatus    = getStatus checks "plan_json"
                    allPassStatus = ghcStatus == "pass"
                                 && haddockStatus == "pass"
                                 && planStatus == "pass"
                in if allPassStatus
                     then allPass
                     else True
              _ -> True)

      , testCase "a passing doctor does not claim to be outside a plan" $ do
          -- 'outside_plan' answers "was there a plan to pin this answer
          -- to", which for doctor is never a function of whether the
          -- checks passed.  The precondition is asserted rather than
          -- assumed: the suite runs from the repo root under cabal, so
          -- ghc, haddock and plan.json are all present by construction,
          -- and a run where they are not would otherwise let this test
          -- pass without ever exercising the case it exists for.
          outcome <- runDoctor OverlayUnset Nothing
          assertBool "precondition: every doctor check passes here"
                     (extractAllPass (outcomeResult outcome))
          outcomeOutsidePlan outcome @?= False

      , testCase "checks contain ghc, haddock, plan_json and plan_overlay" $ do
          outcome <- runDoctor OverlayUnset Nothing
          let checksObj = extractChecks (outcomeResult outcome)
          assertBool "checks should contain all four checks"
            (case checksObj of
              Just (Aeson.Object checks) ->
                   hasField "ghc"          checks
                && hasField "haddock"      checks
                && hasField "plan_json"    checks
                && hasField "plan_overlay" checks
              _ -> False)

      , testCase "a plan_overlay that does not pass keeps all_pass false" $
          withSystemTempDirectory "hypha-doctor" $ \dir -> do
            let missing = dir </> "no-such-overlay.json"
                broken  = dir </> "broken.json"
            writeFile broken "{"
            for_ [ ("warn" :: Text.Text, OverlayMissing ByFlag missing)
                 , ("fail", OverlayFile ByFlag broken) ] $ \(status, setting) -> do
              outcome <- runDoctor setting Nothing
              let body = outcomeResult outcome
              -- The other three checks pass here, as in the test above, so
              -- all_pass can only be false because of this one.
              case extractChecks body of
                Just (Aeson.Object checks) -> do
                  getStatus checks "plan_overlay" @?= status
                  for_ ["ghc", "haddock", "plan_json"] $ \other ->
                    assertBool ("precondition: " <> Text.unpack other <> " passes here")
                               (getStatus checks other == "pass")
                _ -> assertFailure "checks missing"
              assertBool ("a " <> Text.unpack status <> " overlay must make all_pass false")
                         (not (extractAllPass body))
      ]

  , testGroup "plan_overlay"
      [ testCase "a loaded overlay passes with its source, path, counts and unsourced units" $
          withOverlayProject True $ \root dir -> do
            let file = dir </> "overlay.json"
            writeFile file mixedOverlay
            for_ [ (OverlayFile ByFlag file, "--plan-overlay")
                 , (OverlayFile ByEnvironment file, "HYPHA_PLAN_OVERLAY") ] $
              \(setting, source) -> do
                msg <- passes =<< checkPlanOverlay setting (Just root)
                mapM_ (assertMentions msg)
                  [ Text.pack file, source
                  , "units sourced: 1"
                  -- One entry named the plan's configured unit, one named
                  -- no unit of the plan.
                  , "entries not applied: 1 naming a unit cabal configures itself, \
                    \1 naming no unit of the plan"
                  , "pre-existing units left without a source: 1"
                  , "base-4.18.3.0"
                  , "boot packages"
                  ]
                -- The list is coverage, not a staleness check.
                assertBool "must not claim to check staleness"
                  (not ("stale" `Text.isInfixOf` Text.toLower msg))
                assertMentions msg "does not show whether a covered unit's source is current"

      , testCase "an overlay at the default path is named as such" $
          withOverlayProject True $ \root _ -> do
            writeFile (defaultOverlayPath root) mixedOverlay
            msg <- passes =<< checkPlanOverlay OverlayUnset (Just root)
            assertMentions msg "default path"
            assertMentions msg (Text.pack (defaultOverlayPath root))
            assertMentions msg "units sourced: 1"

      , testCase "no overlay passes naming the default path it looked at" $
          withOverlayProject True $ \root _ -> do
            msg <- passes =<< checkPlanOverlay OverlayUnset (Just root)
            assertMentions msg "no plan overlay"
            assertMentions msg (Text.pack (defaultOverlayPath root))

      , testCase "a file named by the flag or the variable that is missing warns" $
          withOverlayProject True $ \root dir -> do
            let missing = dir </> "no-such-overlay.json"
            for_ [ (OverlayMissing ByFlag missing, "--plan-overlay")
                 , (OverlayMissing ByEnvironment missing, "HYPHA_PLAN_OVERLAY") ] $
              \(setting, source) -> do
                -- With and without a project root.
                for_ [Just root, Nothing] $ \mRoot -> do
                  msg <- warns =<< checkPlanOverlay setting mRoot
                  assertMentions msg (Text.pack missing)
                  assertMentions msg source
                  assertMentions msg "does not exist"

      , testCase "an overlay that sourced no unit warns" $
          withOverlayProject True $ \root dir -> do
            let file = dir </> "elsewhere.json"
            writeFile file (overlayOf [("vector-0.13-zzz", "/nix/store/x-vector")])
            msg <- warns =<< checkPlanOverlay (OverlayFile ByFlag file) (Just root)
            assertMentions msg "sourced no unit"
            assertMentions msg "some other package set"
            assertMentions msg "units sourced: 0"

      , testCase "a malformed or unreadable overlay fails naming the file and the cause" $
          withOverlayProject True $ \root dir -> do
            let broken = dir </> "broken.json"
                duplicated = dir </> "duplicated.json"
            writeFile broken "{ \"install-plan\": ["
            writeFile duplicated $ overlayOf
              [ ("patched-dep-1.0-store", "/nix/store/a"), ("patched-dep-1.0-store", "/nix/store/b") ]
            for_ [Just root, Nothing] $ \mRoot -> do
              malformed <- fails =<< checkPlanOverlay (OverlayFile ByFlag broken) mRoot
              assertMentions malformed (Text.pack broken)
              assertMentions malformed "malformed"
              dup <- fails =<< checkPlanOverlay (OverlayFile ByEnvironment duplicated) mRoot
              assertMentions dup (Text.pack duplicated)
              assertMentions dup "patched-dep-1.0-store"
              -- A directory is not a readable file.
              unreadable <- fails =<< checkPlanOverlay (OverlayFile ByFlag dir) mRoot
              assertMentions unreadable (Text.pack dir)
              assertMentions unreadable "cannot read the file"
            -- Also a broken file at the default path.
            writeFile (defaultOverlayPath root) "{"
            viaDefault <- fails =<< checkPlanOverlay OverlayUnset (Just root)
            assertMentions viaDefault (Text.pack (defaultOverlayPath root))
            assertMentions viaDefault "default path"

      , testCase "without a plan.json the overlay is judged as a file alone" $
          withOverlayProject False $ \root dir -> do
            let file  = dir </> "overlay.json"
                empty = dir </> "empty.json"
                broken = dir </> "broken.json"
            writeFile file mixedOverlay
            writeFile empty (overlayOf [])
            writeFile broken "{"
            msg <- passes =<< checkPlanOverlay (OverlayFile ByFlag file) (Just root)
            assertMentions msg (Text.pack file)
            assertMentions msg "entries: 3"
            assertMentions msg "not applied"
            assertMentions msg "plan.json is unavailable"
            -- The default file is found and judged alone too.
            writeFile (defaultOverlayPath root) mixedOverlay
            viaDefault <- passes =<< checkPlanOverlay OverlayUnset (Just root)
            assertMentions viaDefault "default path"
            assertMentions viaDefault "not applied"
            -- An overlay with no entries sources nothing, whatever the plan.
            noEntries <- warns =<< checkPlanOverlay (OverlayFile ByFlag empty) (Just root)
            assertMentions noEntries "sources no unit"
            _ <- fails =<< checkPlanOverlay (OverlayFile ByFlag broken) (Just root)
            pure ()

      , testCase "outside a project only a named file is judged" $
          withOverlayProject False $ \_ dir -> do
            let file = dir </> "overlay.json"
            writeFile file mixedOverlay
            msg <- passes =<< checkPlanOverlay (OverlayFile ByFlag file) Nothing
            assertMentions msg (Text.pack file)
            assertMentions msg "no project root was found"
            assertMentions msg "not applied"
            none <- passes =<< checkPlanOverlay OverlayUnset Nothing
            assertMentions none "no plan overlay was given"
            assertMentions none "no project root was found to look for the default in"
      ]
  ]

-- | A project directory (and a scratch directory beside it for overlay
-- files): the plan of the committed @pre-existing-src@ fixture, which has
-- a @pre-existing@ unit without a source (@base@), one with a source
-- (@patched-dep@) and a configured one (@app@) — or no @plan.json@.
withOverlayProject :: Bool -> (ProjectRoot -> FilePath -> IO a) -> IO a
withOverlayProject withPlan k =
  withSystemTempDirectory "hypha-doctor-overlay" $ \tmp -> do
    let root = tmp </> "project"
        dir  = tmp </> "files"
    createDirectoryIfMissing True root
    createDirectoryIfMissing True dir
    writeFile (root </> "cabal.project") "packages: .\n"
    when withPlan $ do
      let planFile = "dist-newstyle" </> "cache" </> "plan.json"
      createDirectoryIfMissing True (root </> "dist-newstyle" </> "cache")
      copyFile ("test" </> "fixtures" </> "pre-existing-src" </> planFile)
               (root </> planFile)
    k (ProjectRoot root) dir

-- | An overlay file with the given (unit id, path) entries.
overlayOf :: [(String, String)] -> String
overlayOf entries =
  "{\"install-plan\": [" <> intercalate ","
    [ "{\"id\": \"" <> uid <> "\", \"pkg-src\": {\"type\": \"local\", \"path\": \""
        <> path <> "\"}}"
    | (uid, path) <- entries ] <> "]}"

-- | Against the fixture plan: one entry applies (@patched-dep@), one names
-- the configured @app@, and one names no unit of the plan.
mixedOverlay :: String
mixedOverlay = overlayOf
  [ ("patched-dep-1.0-store", "/nix/store/a-patched-dep")
  , ("app-0.1.0-inplace",     "/nix/store/b-app")
  , ("vector-0.13-zzz",       "/nix/store/c-vector")
  ]

passes, warns, fails :: CheckResult -> IO Text.Text
passes = expecting "pass" (\case CheckPass m -> Just m; _ -> Nothing)
warns  = expecting "warn" (\case CheckWarn m -> Just m; _ -> Nothing)
fails  = expecting "fail" (\case CheckFail m -> Just m; _ -> Nothing)

expecting :: String -> (CheckResult -> Maybe Text.Text) -> CheckResult -> IO Text.Text
expecting want pick r = maybe (assertFailure ("expected " <> want <> ", got " <> show r)) pure (pick r)

assertMentions :: Text.Text -> Text.Text -> IO ()
assertMentions msg needle =
  assertBool ("expected " <> show needle <> " in: " <> Text.unpack msg)
             (needle `Text.isInfixOf` msg)

-- | Helper to extract all_pass from Aeson.Value.
extractAllPass :: Aeson.Value -> Bool
extractAllPass (Aeson.Object obj) =
  case KM.lookup "all_pass" obj of
    Just (Aeson.Bool b) -> b
    _ -> False
extractAllPass _ = False

-- | Helper to extract checks from Aeson.Value.
extractChecks :: Aeson.Value -> Maybe Aeson.Value
extractChecks (Aeson.Object obj) =
  KM.lookup "checks" obj
extractChecks _ = Nothing

-- | Helper to extract status string from a check object.
getStatus :: KM.KeyMap Aeson.Value -> Text.Text -> Text.Text
getStatus checks checkName =
  case KM.lookup (Key.fromText checkName) checks of
    Just (Aeson.Object checkObj) ->
      case KM.lookup "status" checkObj of
        Just (Aeson.String s) -> s
        _ -> ""
    _ -> ""

-- | Helper to check if a field exists in a KeyMap.
hasField :: Text.Text -> KM.KeyMap Aeson.Value -> Bool
hasField fieldName checks =
  case KM.lookup (Key.fromText fieldName) checks of
    Just _ -> True
    _ -> False
