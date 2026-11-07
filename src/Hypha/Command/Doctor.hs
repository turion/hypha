{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Environment health check command.
--
-- The @doctor@ command diagnoses the Haskell development environment by checking:
--   * GHC availability on PATH
--   * Haddock availability on PATH
--   * Presence of dist-newstyle/cache/plan.json
--   * The plan overlay in use, if any: where it came from, what it sourced
--     and what it left without a source
--
-- Returns a JSON envelope with check statuses (pass/warn/fail) and an
-- @all_pass@ boolean summarizing the overall health.
module Hypha.Command.Doctor
  ( -- * Field sets
    compactKeys
  , fullKeys
    -- * Execution
  , runDoctor
    -- * Individual checks
  , CheckResult (..)
  , checkPlanOverlay
  ) where

import Control.Exception.Safe (try, SomeException)
import Data.Aeson qualified as Aeson
import Data.Aeson (Value, (.=))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text qualified as Text
import Data.Text (Text)
import Cabal.Plan qualified as CP
import Hypha.Cli.Types
import Hypha.Error (HyphaError (..), errorMessage)
import Hypha.Output.Outcome (Outcome, successOutcome)
import Hypha.Project.Plan
  ( AppliedOverlay (..), MergedPlan (..), OverlaySetting (..)
  , OverlaySource (..), PlanError (..), chosenOverlay, defaultOverlayPath
  , loadMergedPlan, renderOverlayFailure, renderOverlaySource )
import Hypha.Project.PlanOverlay
  ( EntryOutcome (..), OverlayError, OverlayReport (..), PlanOverlay (..)
  , readPlanOverlay )
import Hypha.Types.BuildPlan (ProjectRoot)
import System.Directory (doesFileExist, findExecutable)

compactKeys, fullKeys :: Set Text
compactKeys = Set.fromList ["checks", "all_pass"]
fullKeys    = compactKeys

-- | Result of a single health check.
data CheckResult
  = CheckPass !Text
  | CheckWarn !Text
  | CheckFail !Text
  deriving stock (Show, Eq)

-- | Run all environment checks.
--
-- Takes the plan overlay setting @runHypha@ resolved and the project root
-- discovery found, if any: @hypha doctor@ answers outside a project, where
-- the root is 'Nothing'.
--
-- Note: This returns 'Outcome Value' directly (not 'Either HyphaError')
-- because the checks themselves never fail at the IO boundary; they always
-- produce a structured result (pass/warn/fail) in the outcome body.
runDoctor :: OverlaySetting -> Maybe ProjectRoot -> IO (Outcome Value)
runDoctor overlaySetting mRoot = do
  ghcResult <- checkGhc
  haddockResult <- checkHaddock
  planResult <- checkPlanJson
  overlayResult <- checkPlanOverlay overlaySetting mRoot

  let allPass = and [case r of CheckPass _ -> True; _ -> False | r <- [ghcResult, haddockResult, planResult, overlayResult]]
      checks = Aeson.object
        [ "ghc"          .= checkToJson ghcResult
        , "haddock"      .= checkToJson haddockResult
        , "plan_json"    .= checkToJson planResult
        , "plan_overlay" .= checkToJson overlayResult
        ]
      body = Aeson.object
        [ "checks"   .= checks
        , "all_pass" .= allPass
        ]
  -- Built through 'successOutcome' rather than the positional
  -- constructor: 'Outcome' takes a 'Bool' third, and spelling this out
  -- as @Outcome body DoctorCmd allPass [] mempty@ silently filed
  -- 'allPass' as @outside_plan@, so a healthy doctor reported itself
  -- outside a plan while its own plan_json check said otherwise.
  pure (successOutcome DoctorCmd body)

checkGhc :: IO CheckResult
checkGhc = do
  eGhc <- try @IO @SomeException (findExecutable "ghc")
  case eGhc of
    Left _ -> pure (CheckFail "ghc not found on PATH")
    Right Nothing -> pure (CheckFail "ghc not found on PATH")
    Right (Just path) -> pure (CheckPass ("ghc found at " <> Text.pack path))

checkHaddock :: IO CheckResult
checkHaddock = do
  eHaddock <- try @IO @SomeException (findExecutable "haddock")
  case eHaddock of
    Left _ -> pure (CheckWarn "haddock not found on PATH (documentation generation unavailable)")
    Right Nothing -> pure (CheckWarn "haddock not found on PATH (documentation generation unavailable)")
    Right (Just path) -> pure (CheckPass ("haddock found at " <> Text.pack path))

checkPlanJson :: IO CheckResult
checkPlanJson = do
  exists <- doesFileExist "dist-newstyle/cache/plan.json"
  if exists
    then pure (CheckPass "plan.json found")
    else pure (CheckFail "plan.json not found (run `cabal build --dry-run`)")

-- | The plan overlay in use: where it came from and what it did to the
-- plan.
--
-- With a project root the overlay and @plan.json@ are read together, by
-- 'loadMergedPlan', the step every command loads its plan through, so the
-- report is about the merge those commands see.  When that cannot run (no
-- root, or no readable @plan.json@) the overlay file is read alone and
-- judged as a file; the message says it was not applied.
--
-- An overlay named by the flag or the variable that does not exist is a
-- warning, not a failure: @runHypha@ already continues without one.
checkPlanOverlay :: OverlaySetting -> Maybe ProjectRoot -> IO CheckResult
checkPlanOverlay setting mRoot = case (setting, mRoot) of
  (OverlayMissing how f, _) -> pure $ CheckWarn $
    "plan overlay " <> Text.pack f <> " (from "
      <> renderOverlaySource (OverlayExplicit how)
      <> ") does not exist; no plan overlay applies"
  (_, Just root) -> loadMergedPlan setting root >>= \case
    Right (MergedPlan _ _ (Just applied)) -> pure (judgeApplied applied)
    Right (MergedPlan _ _ Nothing)        -> pure (noOverlay mRoot)
    Left (PlanOverlayFailure source oe)   -> pure (overlayFailed source oe)
    Left planErr -> judgeAlone
      ("plan.json is unavailable: " <> errorMessage (PlanFailure root planErr))
  (_, Nothing) -> judgeAlone "no project root was found, so there is no plan.json"
  where
    -- The file alone, since it cannot be merged into a plan.
    judgeAlone :: Text -> IO CheckResult
    judgeAlone why = chosenOverlay setting mRoot >>= \case
      Nothing -> pure (noOverlay mRoot)
      Just (source, file) -> do
        read' <- readPlanOverlay file
        pure $ case read' of
          Left oe -> overlayFailed source oe
          Right overlay -> judgeUnapplied source file overlay why

-- | Nothing names an overlay.
noOverlay :: Maybe ProjectRoot -> CheckResult
noOverlay = CheckPass . \case
  Just root ->
    "no plan overlay: no --plan-overlay, no HYPHA_PLAN_OVERLAY, and no file at "
      <> Text.pack (defaultOverlayPath root)
  Nothing ->
    "no plan overlay was given, and no project root was found to look for \
    \the default in"

-- | An overlay that could not be read or understood: the file, who named
-- it, and why.
overlayFailed :: OverlaySource -> OverlayError -> CheckResult
overlayFailed source oe = CheckFail (renderOverlayFailure source oe)

-- | An overlay merged into @plan.json@: what it sourced, what it did not
-- apply, and which @pre-existing@ units it left without a source.
--
-- The unsourced units are listed as a coverage report only: a unit keeps
-- its id when a patch or a source changes at the same version, so a stale
-- entry never shows up there.
judgeApplied :: AppliedOverlay -> CheckResult
judgeApplied (AppliedOverlay source file report) =
  (if sourced == 0 then CheckWarn else CheckPass) $
    "plan overlay " <> Text.pack file <> " (from " <> renderOverlaySource source <> ")"
      <> (if sourced == 0
            then " sourced no unit of this plan; it was built for some other package set"
            else " applied")
      <> "; units sourced: " <> tshow sourced
      <> "; entries not applied: " <> tshow configured
      <> " naming a unit cabal configures itself, " <> tshow notInPlan
      <> " naming no unit of the plan (normal: an overlay built from a nix \
         \closure covers more than one plan)"
      <> "; pre-existing units left without a source: "
      <> tshow (Set.size unsourced)
      <> " (GHC's boot packages are always among them; this lists dependencies \
         \the overlay does not cover, and does not show whether a covered \
         \unit's source is current)"
      <> (if Set.null unsourced then "" else ": " <> Text.intercalate ", " unitIds)
  where
    outcomes  = Map.elems (orEntries report)
    sourced   = length (filter (== EntryApplied) outcomes)
    configured = length (filter (== EntryConfiguredUnit) outcomes)
    notInPlan = length (filter (== EntryNotInPlan) outcomes)
    unsourced = orUnsourced report
    unitIds   = [ uid | CP.UnitId uid <- Set.toAscList unsourced ]

-- | An overlay read without a plan to merge it into: judged as a file.
-- One with no entries sources nothing, whatever the plan.
judgeUnapplied :: OverlaySource -> FilePath -> PlanOverlay -> Text -> CheckResult
judgeUnapplied source file (PlanOverlay entries) why =
  (if Map.null entries then CheckWarn else CheckPass) $
    "plan overlay " <> Text.pack file <> " (from " <> renderOverlaySource source <> ")"
      <> " is readable; entries: " <> tshow (Map.size entries)
      <> (if Map.null entries then ", so it sources no unit" else "")
      <> "; it was not applied, since " <> why

tshow :: Show a => a -> Text
tshow = Text.pack . show

checkToJson :: CheckResult -> Value
checkToJson = \case
  CheckPass msg -> Aeson.object
    [ "status" .= ("pass" :: Text)
    , "message" .= msg
    ]
  CheckWarn msg -> Aeson.object
    [ "status" .= ("warn" :: Text)
    , "message" .= msg
    ]
  CheckFail msg -> Aeson.object
    [ "status" .= ("fail" :: Text)
    , "message" .= msg
    ]
