{-# LANGUAGE OverloadedStrings #-}
module Unit.Project (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.Directory
  ( copyFile, createDirectory, createDirectoryIfMissing, makeAbsolute
  , setModificationTime )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)
import qualified Data.Text as Text

import Data.Aeson qualified as Aeson
import Hypha.BuildEnv.Mock (emptyMock, mkMockBuildEnv)
import Hypha.Command.Package (packageOriginToJSON)
import Hypha.Hackage.Api (mkOfflineHackageClient)
import Hypha.Package.Resolver (PackageResolver (..), mkPackageResolver, planSrcDir)
import Hypha.Project.Discovery (DiscoveryError (..), cabalStoreBase)

import Hypha.Types.BuildPlan
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import Hypha.Project.Discovery (discoverProjectRoot)
import Hypha.Error (HyphaError (..), errorActions, errorMessage)
import Hypha.Project.Plan
  ( AppliedOverlay (..), ExplicitOverlay (..), MergedPlan (..)
  , OverlaySetting (..), OverlaySource (..), PlanError (..), PlanPin (..)
  , defaultOverlayPath, loadBuildPlan, loadMergedPlan, loadPlanVersions
  , resolveOverlaySetting )
import Hypha.Project.PlanOverlay
  (EntryOutcome (..), OverlayError (..), OverlayReport (..))
import Hypha.Project.Overrides (parsePackageOverride)

tests :: TestTree
tests = testGroup "Unit.Project"
  [ testDiscovery
  , testDiscoveryIgnoresDotCabalDir
  , testStoreDirFromProjectFiles
  , testLoadBuildPlan
  , testPreExistingSourceDirectory
  , testSourceRepoUnitsResolveToCheckout
  , testLoadPlanVersionsAgrees
  , testParseOverride
  , testApplyOverrides
  , testOverrideDropsSourceDirectory
  , testSourceLookupMatchesPlanVersion
  , testGroup "plan overlay"
      [ testOverlayPrecedence
      , testOverlayDefaultPath
      , testOverlayMissingDoesNotFallBack
      , testOverlaySourcesDependency
      , testOverlayMalformedFailsLoad
      , testPlanVersionsIgnoreOverlay
      ]
  ]

testDiscovery :: TestTree
testDiscovery = testCase "discoverProjectRoot finds tiny-project fixture" $ do
  -- Get the fixture path relative to the test directory
  let fixtureDir = "test" </> "fixtures" </> "tiny-project"
  result <- discoverProjectRoot (Just fixtureDir)
  case result of
    Left err -> error ("Expected to find project root, got: " ++ show err)
    Right (ProjectRoot root) -> do
      -- The root should end with the tiny-project directory
      assertBool "root should contain tiny-project" ("tiny-project" `elem` pathSegments root)
  where
    pathSegments = words . map (\c -> if c == '/' then ' ' else c)

-- | Regression: walking up from a non-project subdir must NOT
-- stop at a directory whose only \"cabal-ish\" entry is a @.cabal@
-- /directory/ (the per-user cabal config dir, common at @~/@).
-- A real cabal project requires either @cabal.project@ or a
-- @<pkg>.cabal@ /file/ with a non-empty stem.
testDiscoveryIgnoresDotCabalDir :: TestTree
testDiscoveryIgnoresDotCabalDir =
  testCase "discoverProjectRoot ignores a `.cabal` config directory" $
    withSystemTempDirectory "hypha-discovery" $ \fakeHome -> do
      -- Mimic ~/ shape: only a `.cabal` directory, no real .cabal file.
      createDirectory (fakeHome </> ".cabal")
      createDirectory (fakeHome </> "subdir")
      result <- discoverProjectRoot (Just (fakeHome </> "subdir"))
      case result of
        Right (ProjectRoot r) ->
          error ("Expected NoProjectFound, found " <> r)
        Left NoProjectFound{} -> pure ()

-- | @store-dir@ is read the way cabal reads it: @cabal.project.local@
-- over @cabal.project@, a relative path against the project root.
-- A project built into a non-default store is otherwise served from
-- whatever @~/.cabal/store@ happens to hold, silently.
testStoreDirFromProjectFiles :: TestTree
testStoreDirFromProjectFiles =
  testCase "cabalStoreBase honours store-dir from the project files" $
    withSystemTempDirectory "hypha-store" $ \root -> do
      writeFile (root </> "cabal.project") $ unlines
        [ "packages: .", "store-dir: /stores/from-project" ]
      fromProject <- cabalStoreBase (Just (ProjectRoot root))
      fromProject @?= "/stores/from-project"

      writeFile (root </> "cabal.project.local") $ unlines
        [ "tests: true", "-- the store this checkout builds into"
        , "store-dir: /stores/from-local", "", "package foo"
        , "  ghc-options: -Wall", "  store-dir: /stores/not-a-thing-here" ]
      fromLocal <- cabalStoreBase (Just (ProjectRoot root))
      fromLocal @?= "/stores/from-local"

      writeFile (root </> "cabal.project.local") "store-dir: .store\n"
      relative <- cabalStoreBase (Just (ProjectRoot root))
      relative @?= root </> ".store"

testLoadBuildPlan :: TestTree
testLoadBuildPlan = testCase "loadBuildPlan parses fixture plan.json" $
  withSystemTempDirectory "hypha-plan" $ \cacheDir -> do
    let fixtureDir = "test" </> "fixtures" </> "tiny-project"
    result <- loadBuildPlan cacheDir OverlayUnset (ProjectRoot fixtureDir)
    case result of
      Left err -> error ("Expected to parse plan, got: " ++ show err)
      Right bp -> do
        -- Check compiler
        bpCompiler bp @?= CompilerId "ghc-9.6.7"
        -- Check packages
        lookupPackage (PackageName "async") bp @?= Just (Version "2.2.5")
        lookupPackage (PackageName "base") bp @?= Just (Version "4.18.3.0")
        lookupPackage (PackageName "text") bp @?= Just (Version "2.0.2")
        lookupPackage (PackageName "nonexistent") bp @?= Nothing
        -- mylib contributes two units (lib + exe:myexe).  The map is
        -- keyed by package name, so the library-carrying unit must be
        -- the one retained: its dist-dir holds the rendered Haddock.
        case lookupUnit (PackageName "mylib") bp of
          Nothing -> error "Expected a unit for mylib"
          Just pu -> puDistDir pu @?= Just
            ("test/fixtures/tiny-project/dist-newstyle/build/x86_64-linux/ghc-9.6.7/mylib-0.1.0")

-- | A @pre-existing@ unit whose plan entry carries @pkg-src@ is a
-- dependency from GHC's package database that says where its source
-- sits.  It must neither become one of the project's own packages nor
-- lose the directory; a @pre-existing@ unit without one is as before.
testPreExistingSourceDirectory :: TestTree
testPreExistingSourceDirectory =
  testCase "loadBuildPlan reads pkg-src on a pre-existing unit as a distribution source directory" $
    withSystemTempDirectory "hypha-plan" $ \cacheDir -> do
      let fixtureDir = "test" </> "fixtures" </> "pre-existing-src"
          srcDir     = fixtureDir </> "patched-dep"
      result <- loadBuildPlan cacheDir OverlayUnset (ProjectRoot fixtureDir)
      case result of
        Left err -> error ("Expected to parse plan, got: " ++ show err)
        Right bp -> do
          case lookupUnit (PackageName "patched-dep") bp of
            Nothing -> error "Expected a unit for patched-dep"
            Just pu -> do
              puSrcDir pu   @?= Just srcDir
              puIsLocal pu  @?= False
              puOrigin pu   @?= OriginDistribution (Just srcDir)
              -- The directory is read like any other source directory.
              assertBool "its .cabal file is parsed for components"
                (not (null (puLibComponents pu)))
              packageOriginToJSON (puOrigin pu) @?= Aeson.object
                [ "kind" Aeson..= ("distribution" :: String)
                , "path" Aeson..= srcDir ]
          -- A pre-existing unit with no pkg-src, and the project's own
          -- package, keep the origins they had.
          fmap puOrigin (lookupUnit (PackageName "base") bp)
            @?= Just (OriginDistribution Nothing)
          fmap puOrigin (lookupUnit (PackageName "app") bp)
            @?= Just (OriginLocal (fixtureDir </> "app"))
          fmap (packageOriginToJSON . puOrigin) (lookupUnit (PackageName "base") bp)
            @?= Just (Aeson.object [ "kind" Aeson..= ("distribution" :: String) ])

-- | A @source-repository-package@ unit's source is cabal's checkout under
-- @dist-newstyle/src@, and the plan must say so, or the resolver falls
-- through to Hackage for a package that was never there.
--
-- Built in a temp dir rather than committed: the checkouts carry a
-- @.git@, which git will not track inside another repository.  The
-- layout mirrors cabal's own — the checkout directory is named after the
-- /repository/, so a subdir package's name appears nowhere in the path;
-- @HEAD@ is the symbolic ref cabal's sync leaves behind, with the commit
-- in a loose or packed ref; a @subdir@ may carry a trailing slash; and
-- re-checkouts of the same repository accumulate, the stale one here
-- being the /newer/ directory so that only the pin can pick right.
testSourceRepoUnitsResolveToCheckout :: TestTree
testSourceRepoUnitsResolveToCheckout =
  testCase "loadBuildPlan resolves source-repo units to their dist-newstyle/src checkout" $
    withSystemTempDirectory "hypha-srp" $ \root -> do
      let src      = root </> "dist-newstyle" </> "src"
          current  = src </> "boolexpr-144ce0325b4bba1a"
          stale    = src </> "boolexpr-stale0000000000"
          monoRepo = src </> "mono-repo-9fa0c3d5e2b17a44"
          inner    = monoRepo </> "pkgs" </> "inner"
          tag      = "bcd7cb20a1b1bc3b58c4ba1b6ae1bccfe62f67ae"
          innerTag = "1111111111111111111111111111111111111111"
      writeFile (root </> "cabal.project") "packages: .\n"
      checkout current  (LooseRef  tag)
      checkout stale    (Detached  "0000000000000000000000000000000000000000")
      checkout monoRepo (PackedRef innerTag)
      cabalFile current "boolexpr" "0.3"
      cabalFile stale   "boolexpr" "0.3"
      cabalFile inner   "inner"    "0.2.0"
      now <- getCurrentTime
      setModificationTime current (addUTCTime (-3600) now)
      setModificationTime stale   now
      createDirectoryIfMissing True (root </> "dist-newstyle" </> "cache")
      writeFile (root </> "dist-newstyle" </> "cache" </> "plan.json") $ unlines
        [ "{ \"cabal-version\": \"3.12.1.0\", \"cabal-lib-version\": \"3.12.1.0\","
        , "  \"compiler-id\": \"ghc-9.6.6\", \"os\": \"linux\", \"arch\": \"x86_64\","
        , "  \"install-plan\": ["
        , srpUnit "boolexpr" "0.3"   "https://github.com/boolexpr/boolexpr.git" tag Nothing <> ","
        , srpUnit "inner"    "0.2.0" "https://example.org/mono-repo.git" innerTag
            (Just "pkgs/inner/")
        , "  ] }"
        ]
      withSystemTempDirectory "hypha-plan" $ \cacheDir -> do
        result <- loadBuildPlan cacheDir OverlayUnset (ProjectRoot root)
        case result of
          Left err -> error ("Expected to parse plan, got: " ++ show err)
          Right bp -> do
            fmap puSrcDir (lookupUnit (PackageName "boolexpr") bp) @?= Just (Just current)
            fmap puSrcDir (lookupUnit (PackageName "inner")    bp) @?= Just (Just inner)
  where
    -- A git checkout as cabal leaves it.
    checkout dir how = do
      createDirectoryIfMissing True (dir </> ".git" </> "refs" </> "heads")
      case how of
        Detached commit  -> writeFile (dir </> ".git" </> "HEAD") (commit <> "\n")
        LooseRef commit  -> do
          writeFile (dir </> ".git" </> "HEAD") "ref: refs/heads/master\n"
          writeFile (dir </> ".git" </> "refs" </> "heads" </> "master") (commit <> "\n")
        PackedRef commit -> do
          writeFile (dir </> ".git" </> "HEAD") "ref: refs/heads/master\n"
          writeFile (dir </> ".git" </> "packed-refs") $ unlines
            [ "# pack-refs with: peeled fully-peeled sorted"
            , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa refs/remotes/origin/master"
            , commit <> " refs/heads/master" ]
    cabalFile dir name ver = do
      createDirectoryIfMissing True dir
      writeFile (dir </> name <> ".cabal") $ unlines
        [ "cabal-version: 3.0", "name: " <> name, "version: " <> ver
        , "build-type: Simple", "", "library", "  build-depends: base"
        , "  default-language: Haskell2010" ]
    srpUnit name ver location commit mSubdir = unlines
      [ "  { \"type\": \"configured\", \"style\": \"global\","
      , "    \"id\": \"" <> name <> "-" <> ver <> "-0000\","
      , "    \"pkg-name\": \"" <> name <> "\", \"pkg-version\": \"" <> ver <> "\","
      , "    \"flags\": {}, \"depends\": [], \"exe-depends\": [], \"component-name\": \"lib\","
      , "    \"pkg-src\": { \"type\": \"source-repo\", \"source-repo\": {"
      , "      \"type\": \"git\", \"location\": \"" <> location <> "\","
      , "      \"tag\": \"" <> commit <> "\""
      , maybe "" (\d -> "      , \"subdir\": \"" <> d <> "\"") mSubdir
      , "    } } }"
      ]

-- | How a fixture checkout records the commit it is at.
data HeadShape = Detached String | LooseRef String | PackedRef String

-- | The cheap reader must agree with the full one, or pinning a lookup
-- to the plan would pin it to a different plan than the one every other
-- command resolves against.
testLoadPlanVersionsAgrees :: TestTree
testLoadPlanVersionsAgrees =
  testCase "loadPlanVersions agrees with the full plan's pins" $
    withSystemTempDirectory "hypha-plan" $ \cacheDir -> do
      let fixtureDir = "test" </> "fixtures" </> "tiny-project"
      full  <- loadBuildPlan     cacheDir OverlayUnset (ProjectRoot fixtureDir)
      light <-                   loadPlanVersions (ProjectRoot fixtureDir)
      case (full, light) of
        (Right bp, Right pins) -> do
          Map.map ppVersion pins @?= planVersions bp
          -- The unit-id too: it is what the index cache keys on, so the
          -- cheap reader disagreeing would scope a lookup to
          -- configurations the rest of hypha never writes.
          Map.map ppUnitId  pins @?= planUnitIds bp
        _ -> error "Expected both plan readers to parse the fixture"

testParseOverride :: TestTree
testParseOverride = testCase "parsePackageOverride parses async=2.2.6" $ do
  let result = parsePackageOverride "async=2.2.6"
  case result of
    Left err -> error ("Expected to parse override, got: " ++ show err)
    Right (PackageOverride name ver) -> do
      name @?= PackageName "async"
      ver @?= Version "2.2.6"

testApplyOverrides :: TestTree
testApplyOverrides = testCase "applyOverrides changes pinned version in plan" $
  withSystemTempDirectory "hypha-plan" $ \cacheDir -> do
    let fixtureDir = "test" </> "fixtures" </> "tiny-project"
    result <- loadBuildPlan cacheDir OverlayUnset (ProjectRoot fixtureDir)
    case result of
      Left err -> error ("Expected to parse plan, got: " ++ show err)
      Right bp -> do
        -- Override async from 2.2.5 to 2.2.6
        let override = PackageOverride (PackageName "async") (Version "2.2.6")
            bp' = applyOverrides [override] bp
        -- Check that the override took effect
        lookupPackage (PackageName "async") bp' @?= Just (Version "2.2.6")
        -- Check that other packages are unchanged
        lookupPackage (PackageName "base") bp' @?= Just (Version "4.18.3.0")
        -- Check that overrides are recorded
        bpOverrides bp' @?= [override]

-- | A source directory the plan named holds the planned version's tree,
-- so an override that moves the unit to another version must not keep
-- it, on the unit or inside a distribution origin.  An override at the
-- unit's own version changes nothing.
testOverrideDropsSourceDirectory :: TestTree
testOverrideDropsSourceDirectory =
  testCase "applyOverrides drops the source directory of a unit whose version it changes" $
    withSystemTempDirectory "hypha-plan" $ \cacheDir -> do
      let fixtureDir = "test" </> "fixtures" </> "pre-existing-src"
          srcDir     = fixtureDir </> "patched-dep"
          patched    = PackageName "patched-dep"
          app        = PackageName "app"
      result <- loadBuildPlan cacheDir OverlayUnset (ProjectRoot fixtureDir)
      case result of
        Left err -> error ("Expected to parse plan, got: " ++ show err)
        Right bp -> do
          let moved = applyOverrides [PackageOverride patched (Version "2.0")] bp
          fmap puSrcDir (lookupUnit patched moved) @?= Just Nothing
          fmap puOrigin (lookupUnit patched moved)
            @?= Just (OriginDistribution Nothing)
          lookupPackage patched moved @?= Just (Version "2.0")
          -- A local unit loses its directory too.
          let movedApp = applyOverrides [PackageOverride app (Version "9.9")] bp
          fmap puSrcDir (lookupUnit app movedApp) @?= Just Nothing
          -- At the unit's own version nothing changes.
          let same = applyOverrides [PackageOverride patched (Version "1.0")] bp
          fmap puSrcDir (lookupUnit patched same) @?= Just (Just srcDir)
          fmap puOrigin (lookupUnit patched same)
            @?= Just (OriginDistribution (Just srcDir))

-- | The first rung of source resolution answers from a plan unit's source
-- directory only at the version the plan pins; at another version it
-- falls through (here to nothing: the build environment and the
-- extracted-source cache are empty), rather than answering with the
-- planned version's tree.
testSourceLookupMatchesPlanVersion :: TestTree
testSourceLookupMatchesPlanVersion =
  testCase "source lookup answers from a plan's source directory only at the plan's version" $
    withSystemTempDirectory "hypha-plan" $ \cacheDir -> do
      let fixtureDir = "test" </> "fixtures" </> "pre-existing-src"
          srcDir     = fixtureDir </> "patched-dep"
          patched    = PackageName "patched-dep"
          planned    = PackageId patched (Version "1.0")
          other      = PackageId patched (Version "2.0")
      result <- loadBuildPlan cacheDir OverlayUnset (ProjectRoot fixtureDir)
      case result of
        Left err -> error ("Expected to parse plan, got: " ++ show err)
        Right bp -> do
          planSrcDir bp planned @?= Just srcDir
          planSrcDir bp other   @?= Nothing
          client   <- mkOfflineHackageClient cacheDir
          resolver <- mkPackageResolver (mkMockBuildEnv emptyMock) client cacheDir bp
          resolveSrcLocal resolver planned >>= (@?= Just srcDir)
          resolveSrcLocal resolver other   >>= (@?= Nothing)
          -- The same request after --package-override at that version.
          let overridden = applyOverrides [PackageOverride patched (Version "2.0")] bp
          resolver' <- mkPackageResolver (mkMockBuildEnv emptyMock) client cacheDir overridden
          resolveSrcLocal resolver' other >>= (@?= Nothing)

-- ---------------------------------------------------------------------------
-- Plan overlay

-- | The committed fixture: a plan whose one dependency has no @pkg-src@,
-- and an overlay that gives it one.
overlayFixture, overlayFile :: FilePath
overlayFixture = "test" </> "fixtures" </> "overlay-project"
overlayFile    = overlayFixture </> "plan-overlay.json"

-- | A throwaway project holding the fixture's plan, so a test can put an
-- overlay at the default path (or break one there) without touching the
-- committed fixture.
withOverlayProject :: (FilePath -> IO a) -> IO a
withOverlayProject k = withSystemTempDirectory "hypha-overlay-project" $ \root -> do
  writeFile (root </> "cabal.project") "packages: .\n"
  createDirectoryIfMissing True (root </> "dist-newstyle" </> "cache")
  let planFile = "dist-newstyle" </> "cache" </> "plan.json"
  copyFile (overlayFixture </> planFile) (root </> planFile)
  k root

-- | How many units the merge applied an entry to.
appliedCount :: AppliedOverlay -> Int
appliedCount ao = length
  [ () | EntryApplied <- Map.elems (orEntries (aoReport ao)) ]

-- | The flag wins over the variable, and either names a file whether or
-- not it exists: a named file that is missing is not replaced by the
-- other name, and the variable only counts when the flag is absent.
testOverlayPrecedence :: TestTree
testOverlayPrecedence = testCase "the flag wins over the variable, which wins over nothing" $ do
  let present = overlayFile
      absent  = overlayFixture </> "no-such-overlay.json"
      absent2 = overlayFixture </> "also-no-such-overlay.json"
  resolveOverlaySetting (Just present) (Just absent)
    >>= (@?= OverlayFile ByFlag present)
  resolveOverlaySetting Nothing (Just present)
    >>= (@?= OverlayFile ByEnvironment present)
  resolveOverlaySetting Nothing Nothing
    >>= (@?= OverlayUnset)
  -- A flag naming a missing file is not displaced by a variable naming a
  -- present one.
  resolveOverlaySetting (Just absent) (Just present)
    >>= (@?= OverlayMissing ByFlag absent)
  resolveOverlaySetting (Just absent) (Just absent2)
    >>= (@?= OverlayMissing ByFlag absent)
  resolveOverlaySetting Nothing (Just absent2)
    >>= (@?= OverlayMissing ByEnvironment absent2)
  -- A variable that is set but empty names nothing.
  resolveOverlaySetting Nothing (Just "")
    >>= (@?= OverlayUnset)

-- | Only when nothing names a file is the default tried, and it is
-- resolved against the project root the load is given, not the working
-- directory.
testOverlayDefaultPath :: TestTree
testOverlayDefaultPath = testCase "the default overlay is <project root>/hypha-plan-overlay.json" $ do
  defaultOverlayPath (ProjectRoot "/some/root")
    @?= "/some/root" </> "hypha-plan-overlay.json"
  withOverlayProject $ \root -> do
    -- No file there: no overlay.
    absentCase <- loadMergedPlan OverlayUnset (ProjectRoot root)
    case absentCase of
      Right mp -> assertBool "no file means no overlay" (null (mpOverlay mp))
      Left err -> assertFailure ("expected the plan to load, got: " <> show err)
    -- A file there: it is the overlay, and says it is the default.
    copyFile overlayFile (defaultOverlayPath (ProjectRoot root))
    present <- loadMergedPlan OverlayUnset (ProjectRoot root)
    case present of
      Right MergedPlan { mpOverlay = Just ao } -> do
        aoSource ao @?= OverlayDefault
        aoPath ao   @?= defaultOverlayPath (ProjectRoot root)
        appliedCount ao @?= 1
      other -> assertFailure ("expected the default overlay to apply, got: "
                                <> either show (const "a plan without one") other)
    -- A file the flag names takes its place, even with a default there.
    flagged <- loadMergedPlan (OverlayFile ByFlag overlayFile) (ProjectRoot root)
    case flagged of
      Right MergedPlan { mpOverlay = Just ao } -> do
        aoSource ao @?= OverlayExplicit ByFlag
        aoPath ao   @?= overlayFile
      other -> assertFailure ("expected the flag's overlay to apply, got: "
                                <> either show (const "a plan without one") other)

-- | A file named explicitly but missing applies no overlay, and the
-- default file beside the plan is not substituted for it.
testOverlayMissingDoesNotFallBack :: TestTree
testOverlayMissingDoesNotFallBack =
  testCase "a named overlay that is missing applies no overlay, not the default" $
    withOverlayProject $ \root -> do
      copyFile overlayFile (defaultOverlayPath (ProjectRoot root))
      r <- loadMergedPlan (OverlayMissing ByEnvironment "gone.json") (ProjectRoot root)
      case r of
        Right mp -> assertBool "no overlay should be in use" (null (mpOverlay mp))
        Left err -> assertFailure ("expected the plan to load, got: " <> show err)

-- | The point of the overlay: a pre-existing unit with no source gets one,
-- and stays a dependency whose source is read like any other.
testOverlaySourcesDependency :: TestTree
testOverlaySourcesDependency =
  testCase "an overlay gives a pre-existing unit its source directory" $
    withSystemTempDirectory "hypha-plan" $ \cacheDir -> do
      let root     = ProjectRoot overlayFixture
          storeDep = PackageName "store-dep"
          srcDir   = overlayFixture </> "store-dep"
      without <- loadBuildPlan cacheDir OverlayUnset root
      case without of
        Left err -> assertFailure ("plan without overlay: " <> show err)
        Right bp -> fmap puSrcDir (lookupUnit storeDep bp) @?= Just Nothing
      with <- loadBuildPlan cacheDir (OverlayFile ByFlag overlayFile) root
      case with of
        Left err -> assertFailure ("plan with overlay: " <> show err)
        Right bp -> case lookupUnit storeDep bp of
          Nothing -> assertFailure "expected a unit for store-dep"
          Just pu -> do
            puSrcDir pu  @?= Just srcDir
            puIsLocal pu @?= False
            puOrigin pu  @?= OriginDistribution (Just srcDir)
            assertBool "its .cabal file is parsed for components"
              (not (null (puLibComponents pu)))
      -- The merge's report and the plan's own path travel with the plan.
      merged <- loadMergedPlan (OverlayFile ByFlag overlayFile) root
      case merged of
        Right mp -> do
          expected <- makeAbsolute
            (overlayFixture </> "dist-newstyle" </> "cache" </> "plan.json")
          mpPlanPath mp @?= expected
          fmap appliedCount (mpOverlay mp) @?= Just 1
        Left err -> assertFailure ("merged plan: " <> show err)

-- | A broken overlay fails the load as a broken @plan.json@ does, with the
-- typed cause intact until the message is rendered, and the message names
-- the file and the cause.
testOverlayMalformedFailsLoad :: TestTree
testOverlayMalformedFailsLoad =
  testCase "a malformed overlay fails the load with an error naming the file" $
    withOverlayProject $ \root -> do
      let file = defaultOverlayPath (ProjectRoot root)
      writeFile file "{ \"install-plan\": 3 }"
      r <- loadMergedPlan OverlayUnset (ProjectRoot root)
      case r of
        Right _ -> assertFailure "a malformed overlay must fail the load"
        Left err@(PlanOverlayFailure source (OverlayMalformed f cause)) -> do
          source @?= OverlayDefault
          f      @?= file
          let message = Text.unpack (errorMessage (PlanFailure (ProjectRoot root) err))
          assertBool ("the message names the file: " <> message) (file `isInfixOf` message)
          assertBool ("the message carries the cause: " <> message) (cause `isInfixOf` message)
          assertBool ("the message says where it came from: " <> message)
            ("default path" `isInfixOf` message)
          Map.lookup "plan_overlay" (errorActions (PlanFailure (ProjectRoot root) err))
            @?= Just (Text.pack file)
        Left other -> assertFailure ("expected an overlay failure, got: " <> show other)
      -- The same file is a failure through 'loadBuildPlan', the way a
      -- broken plan.json is.
      withSystemTempDirectory "hypha-plan" $ \cacheDir -> do
        built <- loadBuildPlan cacheDir OverlayUnset (ProjectRoot root)
        case built of
          Left PlanOverlayFailure{} -> pure ()
          other -> assertFailure ("expected the build plan to fail, got: "
                                    <> either show (const "a plan") other)

-- | The @lookup@ fast path reads versions and unit-ids, which an overlay
-- never changes, so a broken overlay must not be able to fail it.
testPlanVersionsIgnoreOverlay :: TestTree
testPlanVersionsIgnoreOverlay =
  testCase "loadPlanVersions does not read the overlay" $
    withOverlayProject $ \root -> do
      writeFile (defaultOverlayPath (ProjectRoot root)) "this is not JSON"
      r <- loadPlanVersions (ProjectRoot root)
      case r of
        Right pins -> Map.member (PackageName "store-dep") pins @?= True
        Left err   -> assertFailure ("expected the versions to load, got: " <> show err)
