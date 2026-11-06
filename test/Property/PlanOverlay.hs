{-# LANGUAGE OverloadedStrings #-}
-- | Merging an overlay into a plan: which units gain a @pkg-src@, that
-- nothing else moves, and that the report accounts for every entry.
module Property.PlanOverlay (tests) where

import Control.Applicative ((<|>))
import Data.Containers.ListUtils (nubOrd)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as Range
import Test.Falsify.Property (assert, gen)

import qualified Cabal.Plan as CP

import Hypha.Project.PlanOverlay
  ( EntryOutcome (..), OverlayReport (..), PlanOverlay (..), applyPlanOverlay )

-- | Ids from a small alphabet, so overlay entries and plan units
-- actually meet instead of never overlapping.
genId :: Gen.Gen Text
genId = Gen.elem (NE.fromList [ "u" <> Text.pack (show i) | i <- [0 .. 7 :: Int] ])

-- | A unit of any type, with or without a @pkg-src@ of its own.  Its
-- source lives under @\/plan\/@, an overlay's under @\/overlay\/@, so the
-- two never coincide by accident.
genUnit :: Text -> Gen.Gen CP.Unit
genUnit name = do
  ty     <- Gen.elem (NE.fromList
              [ CP.UnitTypeBuiltin, CP.UnitTypeGlobal, CP.UnitTypeLocal, CP.UnitTypeInplace ])
  hasSrc <- Gen.bool False
  pure CP.Unit
    { CP.uId          = CP.UnitId name
    , CP.uPId         = CP.PkgId (CP.PkgName name) (CP.Ver [1])
    , CP.uType        = ty
    , CP.uSha256      = Nothing
    , CP.uCabalSha256 = Nothing
    , CP.uComps       = Map.empty
    , CP.uFlags       = Map.empty
    , CP.uDistDir     = Nothing
    , CP.uPkgSrc      = if hasSrc
                          then Just (CP.LocalUnpackedPackage ("/plan/" <> Text.unpack name))
                          else Nothing
    }

genPlan :: Gen.Gen CP.PlanJson
genPlan = do
  names <- Gen.list (Range.between (0, 8)) genId
  units <- traverse genUnit (nubOrd names)
  pure CP.PlanJson
    { CP.pjCabalVersion    = CP.Ver [3, 14]
    , CP.pjCabalLibVersion = CP.Ver [3, 14]
    , CP.pjCompilerId      = CP.PkgId (CP.PkgName "ghc") (CP.Ver [9, 10, 3])
    , CP.pjArch            = "x86_64"
    , CP.pjOs              = "linux"
    , CP.pjUnits           = Map.fromList [ (CP.uId u, u) | u <- units ]
    }

genOverlay :: Gen.Gen PlanOverlay
genOverlay = do
  names <- Gen.list (Range.between (0, 8)) genId
  pure (PlanOverlay (Map.fromList
    [ (CP.UnitId n, CP.LocalUnpackedPackage ("/overlay/" <> Text.unpack n)) | n <- names ]))

-- | A unit with its @pkg-src@ blanked, in a form that compares: 'CP.Unit'
-- has 'Show' but no 'Eq'.
sourceless :: CP.Unit -> String
sourceless u = show u { CP.uPkgSrc = Nothing }

tests :: TestTree
tests = testGroup "Property.PlanOverlay"
  [ testProperty "no unit changes but in its pkg-src" $ do
      pj      <- gen genPlan
      overlay <- gen genOverlay
      let merged = fst (applyPlanOverlay overlay pj)
      assert (P.eq P..$ ("plan", Map.map sourceless (CP.pjUnits pj))
                   P..$ ("merged", Map.map sourceless (CP.pjUnits merged)))

  , testProperty "pkg-src is set on exactly the pre-existing units the overlay names" $ do
      pj      <- gen genPlan
      overlay <- gen genOverlay
      let merged = fst (applyPlanOverlay overlay pj)
          entries = overlayEntries overlay
          expected = Map.mapWithKey
            (\uid u -> if CP.uType u == CP.UnitTypeBuiltin
                         then Map.lookup uid entries <|> CP.uPkgSrc u
                         else CP.uPkgSrc u)
            (CP.pjUnits pj)
      assert (P.eq P..$ ("expected", expected)
                   P..$ ("merged", Map.map CP.uPkgSrc (CP.pjUnits merged)))

  , testProperty "every entry is accounted for once, as applied, configured or not in the plan" $ do
      pj      <- gen genPlan
      overlay <- gen genOverlay
      let report   = snd (applyPlanOverlay overlay pj)
          named    = Map.keysSet (overlayEntries overlay)
          inPlan   = Map.keysSet (CP.pjUnits pj)
          builtin  = Map.keysSet (Map.filter ((== CP.UnitTypeBuiltin) . CP.uType) (CP.pjUnits pj))
          outcome o = Map.keysSet (Map.filter (== o) (orEntries report))
      assert (P.eq P..$ ("entries", named)
                   P..$ ("accounted for", Map.keysSet (orEntries report)))
      assert (P.eq P..$ ("named pre-existing units", named `Set.intersection` builtin)
                   P..$ ("applied", outcome EntryApplied))
      assert (P.eq P..$ ("named configured units", (named `Set.intersection` inPlan) Set.\\ builtin)
                   P..$ ("configured", outcome EntryConfiguredUnit))
      assert (P.eq P..$ ("named, not in plan", named Set.\\ inPlan)
                   P..$ ("not in plan", outcome EntryNotInPlan))

  , testProperty "the report lists exactly the pre-existing units left without a pkg-src" $ do
      pj      <- gen genPlan
      overlay <- gen genOverlay
      let report  = snd (applyPlanOverlay overlay pj)
          bare    = Map.keysSet (Map.filter
                      (\u -> CP.uType u == CP.UnitTypeBuiltin && CP.uPkgSrc u == Nothing)
                      (CP.pjUnits pj))
      assert (P.eq P..$ ("plan's, not named", bare Set.\\ Map.keysSet (overlayEntries overlay))
                   P..$ ("unsourced", orUnsourced report))
  ]
