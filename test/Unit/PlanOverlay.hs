{-# LANGUAGE OverloadedStrings #-}
-- | Reading a plan overlay: the format, and the typed ways a file can fail
-- to be one.  The merge is covered by "Property.PlanOverlay".
module Unit.PlanOverlay (tests) where

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import qualified Cabal.Plan as CP

import Hypha.Project.PlanOverlay
  ( OverlayError (..), PlanOverlay (..), decodePlanOverlay, readPlanOverlay )

tests :: TestTree
tests = testGroup "Unit.PlanOverlay"
  [ testCase "an overlay in the stated format parses" $
      decodePlanOverlay "o.json"
        "{\"install-plan\": [\
        \  {\"id\": \"aeson-2.2.4.1-3XJY\", \"pkg-src\": {\"type\": \"local\", \"path\": \"/nix/store/a-aeson\"}},\
        \  {\"id\": \"vector-0.13.2.0-ELhk\", \"pkg-src\": {\"type\": \"local\", \"path\": \"/nix/store/b-vector\"}}\
        \]}"
        @?= Right (PlanOverlay (Map.fromList
          [ (CP.UnitId "aeson-2.2.4.1-3XJY", CP.LocalUnpackedPackage "/nix/store/a-aeson")
          , (CP.UnitId "vector-0.13.2.0-ELhk", CP.LocalUnpackedPackage "/nix/store/b-vector")
          ]))

  , testCase "an empty install-plan is an empty overlay" $
      decodePlanOverlay "o.json" "{\"install-plan\": []}"
        @?= Right (PlanOverlay Map.empty)

  , testCase "entries and a top level carrying further plan.json fields parse too" $
      decodePlanOverlay "o.json"
        "{\"cabal-version\": \"3.14.1.1\", \"compiler-id\": \"ghc-9.10.3\", \"install-plan\": [\
        \  {\"type\": \"pre-existing\", \"id\": \"aeson-2.2.4.1-3XJY\", \"pkg-name\": \"aeson\",\
        \   \"pkg-version\": \"2.2.4.1\", \"depends\": [\"base-4.20.0.0\"],\
        \   \"pkg-src\": {\"type\": \"local\", \"path\": \"/nix/store/a-aeson\"}}\
        \]}"
        @?= Right (PlanOverlay (Map.singleton
              (CP.UnitId "aeson-2.2.4.1-3XJY") (CP.LocalUnpackedPackage "/nix/store/a-aeson")))

  , testCase "a pkg-src of another type parses through cabal-plan's PkgLoc instance" $
      decodePlanOverlay "o.json"
        "{\"install-plan\": [\
        \  {\"id\": \"x-1\", \"pkg-src\": {\"type\": \"local-tar\", \"path\": \"/srv/x-1.tar.gz\"}}\
        \]}"
        @?= Right (PlanOverlay (Map.singleton
              (CP.UnitId "x-1") (CP.LocalTarballPackage "/srv/x-1.tar.gz")))

  , testGroup "malformed"
      [ testCase "JSON that is not JSON" $
          assertMalformed (decodePlanOverlay "o.json" "{\"install-plan\": [")
      , testCase "no install-plan key" $
          assertMalformed (decodePlanOverlay "o.json" "{}")
      , testCase "an entry without a pkg-src, naming the key" $
          assertMalformedMentioning "pkg-src"
            (decodePlanOverlay "o.json" "{\"install-plan\": [{\"id\": \"x-1\"}]}")
      , testCase "an entry without an id, naming the key" $
          assertMalformedMentioning "id"
            (decodePlanOverlay "o.json"
              "{\"install-plan\": [{\"pkg-src\": {\"type\": \"local\", \"path\": \"/p\"}}]}")
      , testCase "a pkg-src of an unknown type" $
          assertMalformed (decodePlanOverlay "o.json"
            "{\"install-plan\": [{\"id\": \"x-1\", \"pkg-src\": {\"type\": \"bogus\"}}]}")
      ]

  , testCase "two entries sharing an id are reported, each id once, with the file" $
      decodePlanOverlay "dup.json"
        "{\"install-plan\": [\
        \  {\"id\": \"b-1\", \"pkg-src\": {\"type\": \"local\", \"path\": \"/p1\"}},\
        \  {\"id\": \"a-1\", \"pkg-src\": {\"type\": \"local\", \"path\": \"/p2\"}},\
        \  {\"id\": \"b-1\", \"pkg-src\": {\"type\": \"local\", \"path\": \"/p3\"}},\
        \  {\"id\": \"b-1\", \"pkg-src\": {\"type\": \"local\", \"path\": \"/p4\"}},\
        \  {\"id\": \"a-1\", \"pkg-src\": {\"type\": \"local\", \"path\": \"/p5\"}}\
        \]}"
        @?= Left (OverlayDuplicateIds "dup.json"
              (NE.fromList [CP.UnitId "a-1", CP.UnitId "b-1"]))

  , testCase "readPlanOverlay reads a file" $
      withSystemTempDirectory "hypha-overlay" $ \dir -> do
        let file = dir </> "hypha-plan-overlay.json"
        BS.writeFile file
          "{\"install-plan\": [{\"id\": \"x-1\", \"pkg-src\": {\"type\": \"local\", \"path\": \"/p\"}}]}"
        got <- readPlanOverlay file
        got @?= Right (PlanOverlay (Map.singleton
                  (CP.UnitId "x-1") (CP.LocalUnpackedPackage "/p")))

  , testCase "readPlanOverlay on a file that cannot be read names the file" $
      withSystemTempDirectory "hypha-overlay" $ \dir -> do
        let file = dir </> "absent.json"
        got <- readPlanOverlay file
        case got of
          Left (OverlayUnreadable f _) -> f @?= file
          other -> assertFailure ("expected OverlayUnreadable, got " <> show other)

  , testCase "readPlanOverlay on malformed content names the file" $
      withSystemTempDirectory "hypha-overlay" $ \dir -> do
        let file = dir </> "bad.json"
        BS.writeFile file "not json"
        got <- readPlanOverlay file
        case got of
          Left (OverlayMalformed f _) -> f @?= file
          other -> assertFailure ("expected OverlayMalformed, got " <> show other)
  ]

assertMalformed :: Either OverlayError PlanOverlay -> IO ()
assertMalformed = assertMalformedMentioning ""

-- | The parse fails with 'OverlayMalformed', naming the file and carrying
-- the parser's message, which mentions the given text.
assertMalformedMentioning :: String -> Either OverlayError PlanOverlay -> IO ()
assertMalformedMentioning needle got = case got of
  Left (OverlayMalformed f msg) -> do
    f @?= "o.json"
    assertBool ("message " <> show msg <> " mentions " <> show needle)
               (needle `isInfixOf` msg)
  other -> assertFailure ("expected OverlayMalformed, got " <> show other)
