{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A /plan overlay/: the @pkg-src@ of units whose source @plan.json@ does
-- not name, supplied in a file of its own and merged into the decoded plan.
--
-- A project whose dependencies come from a distribution's package set
-- (a nixpkgs Haskell package set, say) gets every one of them as a
-- @pre-existing@ unit with no @pkg-src@: cabal never fetched them, so it
-- never recorded where their source lives.  Whoever built that package
-- set knows, and writes it down here in @plan.json@'s own vocabulary:
--
-- > { "install-plan":
-- >     [ { "id": "aeson-2.2.4.1-3XJYKs…", "pkg-src": { "type": "local", "path": "/nix/store/…-aeson" } } ] }
--
-- Only those two keys are needed; any other key of a @plan.json@ unit is
-- accepted and ignored, so a fragment of a real @plan.json@ is a valid
-- overlay.  Both values are read with @cabal-plan@'s own 'CP.FromJSON'
-- instances, so the merged 'CP.PlanJson' is exactly what @plan.json@
-- would have decoded to had cabal recorded those sources.
module Hypha.Project.PlanOverlay
  ( -- * The overlay
    PlanOverlay (..)
  , OverlayError (..)
  , readPlanOverlay
  , decodePlanOverlay
    -- * Merging
  , applyPlanOverlay
  , OverlayReport (..)
  , EntryOutcome (..)
  ) where

import Control.Exception.Safe (IOException, try)
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)

import qualified Cabal.Plan as CP

-- | What an overlay supplies: a source location per unit id.
--
-- A map, so one id names one source by construction; a file that names an
-- id twice is rejected at the reader ('OverlayDuplicateIds') rather than
-- resolved by picking an entry.
newtype PlanOverlay = PlanOverlay
  { overlayEntries :: Map CP.UnitId CP.PkgLoc }
  deriving stock (Show, Eq)

-- | Why an overlay file could not be turned into a 'PlanOverlay'.  Each
-- constructor names the file; rendering is the caller's, at the edge.
data OverlayError
  = OverlayUnreadable !FilePath !IOException
    -- ^ The file could not be read.
  | OverlayMalformed !FilePath !String
    -- ^ The JSON, or its shape, is wrong; carries the parser's message.
  | OverlayDuplicateIds !FilePath !(NonEmpty CP.UnitId)
    -- ^ Two or more entries share an id; lists each such id once,
    -- ascending.
  deriving stock (Show, Eq)

-- | One entry of the overlay's @install-plan@ array, before the entries
-- are checked for duplicate ids.
data Entry = Entry !CP.UnitId !CP.PkgLoc

instance Aeson.FromJSON Entry where
  parseJSON = Aeson.withObject "install-plan entry" $ \o ->
    Entry <$> o .: "id" <*> o .: "pkg-src"

-- | The overlay file's top level.
newtype Entries = Entries [Entry]

instance Aeson.FromJSON Entries where
  parseJSON = Aeson.withObject "plan overlay" $ \o ->
    Entries <$> o .: "install-plan"

-- | Read an overlay from a file.
readPlanOverlay :: FilePath -> IO (Either OverlayError PlanOverlay)
readPlanOverlay file = do
  read' <- try @IO @IOException (BS.readFile file)
  pure (first (OverlayUnreadable file) read' >>= decodePlanOverlay file)

-- | Parse an overlay from its bytes.  The 'FilePath' only names the
-- source in an error; nothing is read from it.
decodePlanOverlay :: FilePath -> BS.ByteString -> Either OverlayError PlanOverlay
decodePlanOverlay file bytes = do
  Entries entries <- first (OverlayMalformed file) (Aeson.eitherDecodeStrict' bytes)
  let counts = Map.fromListWith (+) [ (uid, 1 :: Int) | Entry uid _ <- entries ]
  case NE.nonEmpty [ uid | (uid, n) <- Map.toAscList counts, n > 1 ] of
    Just duplicated -> Left (OverlayDuplicateIds file duplicated)
    Nothing -> Right (PlanOverlay (Map.fromList [ (uid, src) | Entry uid src <- entries ]))

-- | What became of one overlay entry.
data EntryOutcome
  = EntryApplied
    -- ^ The plan has a @pre-existing@ unit with this id; it now has the
    -- entry's @pkg-src@.
  | EntryConfiguredUnit
    -- ^ The plan has a unit with this id, but cabal configures it itself
    -- (it builds, and so already knows, its source); the entry is not
    -- applied.
  | EntryNotInPlan
    -- ^ The plan has no unit with this id.  Normal: a distribution's
    -- closure is a superset of the plan, so a current overlay has these.
  deriving stock (Show, Eq)

-- | What 'applyPlanOverlay' did.
data OverlayReport = OverlayReport
  { orEntries   :: !(Map CP.UnitId EntryOutcome)
    -- ^ One outcome per overlay entry, so none is accounted for twice or
    -- left out.
  , orUnsourced :: !(Set CP.UnitId)
    -- ^ The merged plan's @pre-existing@ units that still have no
    -- @pkg-src@.  GHC's boot packages are always among them; any other is
    -- a dependency the overlay does not cover, which is what tells a stale
    -- overlay from a current one.
  }
  deriving stock (Show, Eq)

-- | Merge an overlay into a decoded plan, by unit id.
--
-- Sets @pkg-src@ on each @pre-existing@ unit ('CP.UnitTypeBuiltin') an
-- entry names, replacing one it already had; every other unit is left as
-- it was.
applyPlanOverlay :: PlanOverlay -> CP.PlanJson -> (CP.PlanJson, OverlayReport)
applyPlanOverlay (PlanOverlay entries) pj = (merged, report)
  where
    units = CP.pjUnits pj
    merged = pj { CP.pjUnits = Map.mapWithKey source units }
    source uid unit = case Map.lookup uid entries of
      Just src | CP.uType unit == CP.UnitTypeBuiltin -> unit { CP.uPkgSrc = Just src }
      _ -> unit
    report = OverlayReport
      { orEntries   = Map.mapWithKey (\uid _ -> outcome (Map.lookup uid units)) entries
      , orUnsourced = Map.keysSet (Map.filter unsourced (CP.pjUnits merged))
      }
    outcome Nothing = EntryNotInPlan
    outcome (Just unit)
      | CP.uType unit == CP.UnitTypeBuiltin = EntryApplied
      | otherwise = EntryConfiguredUnit
    unsourced unit = CP.uType unit == CP.UnitTypeBuiltin && CP.uPkgSrc unit == Nothing
