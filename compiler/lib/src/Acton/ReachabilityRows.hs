-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE DeriveGeneric #-}

-- | Persisted, exactly addressable facts used by reachability closure.
--
-- These rows are the semantic counterpart of 'InterfaceRows': interface rows
-- hold independently loadable syntax, while reachability rows say which other
-- top-level names, members, constructors and generated slots that syntax
-- needs.  'Reachability.prepareReachabilityRows' prepares the rows during the
-- front pass, 'InterfaceFiles' stores them in TYDB, and
-- 'Reachability.selectProgram' reads only the exact rows reached from the
-- program roots.
--
-- This module owns the persisted representation, not syntax traversal,
-- whole-program closure, or interface-file IO.
module Acton.ReachabilityRows
  ( MemberRef(..)
  , ReachEdge(..)
  , ReachSummary
  , reachEdges
  , reachSummaryFromEdges
  , singletonReach
  , TopKey(..)
  , TopInfo(..)
  , ShapeKind(..)
  , ConstructorDecl(..)
  , SlotDecl(..)
  , MemberInfo(..)
  , ShapeInfo(..)
  , SlotInfo(..)
  , ReflectableAttrs(..)
  , ReachabilityRows(..)
  , emptyReachabilityRows
  ) where

import qualified Acton.InterfaceRows as Rows
import qualified Acton.NameInfo as I
import qualified Acton.Syntax as A

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import qualified Data.Persist as Persist
import qualified Data.Set as Set
import GHC.Generics (Generic)


-- Dependencies -----------------------------------------------------------------------------------------

data MemberRef = MethodRef A.Name | AttrRef A.Name
                 deriving (Eq, Ord, Show, Read, Generic)

instance NFData MemberRef
instance Persist.Persist MemberRef

data ReachEdge = Declare A.ModName A.Name
               | Need A.ModName A.Name
               | Inherit A.ModName A.Name
               | Construct A.ModName A.Name
               | Direct A.ModName A.Name MemberRef
               | Dispatch A.ModName A.Name MemberRef
               | Reflect A.ModName A.Name
               | DynamicSerialization
               | DeclareAttr A.ModName A.Name A.Name
                 deriving (Eq, Ord, Show, Read, Generic)

instance NFData ReachEdge
instance Persist.Persist ReachEdge

newtype ReachSummary = ReachSummary { reachEdgeSet :: Set.Set ReachEdge }
                       deriving (Eq, Show, Generic)

instance NFData ReachSummary
instance Persist.Persist ReachSummary

instance Semigroup ReachSummary where
    ReachSummary es <> ReachSummary es' = ReachSummary (Set.union es es')

instance Monoid ReachSummary where
    mempty                              = ReachSummary Set.empty

reachEdges                         :: ReachSummary -> [ReachEdge]
reachEdges (ReachSummary edges)    = Set.toAscList edges

reachSummaryFromEdges              :: [ReachEdge] -> ReachSummary
reachSummaryFromEdges              = ReachSummary . Set.fromList

singletonReach                    :: ReachEdge -> ReachSummary
singletonReach edge               = ReachSummary (Set.singleton edge)


-- Persisted row payloads --------------------------------------------------------------------------------

data TopKey = TopKey A.ModName A.Name
              deriving (Eq, Ord, Show, Generic)

instance NFData TopKey
instance Persist.Persist TopKey

data TopInfo = LocalTop (Maybe I.NameInfo) ReachSummary | OpaqueTop ReachSummary
               deriving (Eq, Show, Generic)

instance NFData TopInfo
instance Persist.Persist TopInfo

data ShapeKind = ClassShape | ActorShape | WitnessShape | ProtocolShape
                 deriving (Eq, Ord, Show, Generic)

instance NFData ShapeKind
instance Persist.Persist ShapeKind

data ConstructorDecl
  = StoredConstructor ReachSummary
  | GeneratedConstructor ReachSummary
  | InheritedConstructor ReachSummary
  | OpaqueConstructor
    deriving (Eq, Show, Generic)

instance NFData ConstructorDecl
instance Persist.Persist ConstructorDecl

data SlotDecl
  = StoredSlot Rows.MemberKey
  | AttributeSlot
  | AbstractSlot
  | GeneratedSlot ReachSummary
  | OpaqueSlot
    deriving (Eq, Show, Generic)

instance NFData SlotDecl
instance Persist.Persist SlotDecl

data MemberInfo = MemberInfo
  { memberSummary             :: ReachSummary
  , memberStaticInitSummary   :: Maybe ReachSummary
  , memberInstanceInitSummary :: Maybe ReachSummary
  } deriving (Eq, Show, Generic)

instance NFData MemberInfo
instance Persist.Persist MemberInfo

-- Effective slots and reflectable attributes deliberately live in their own
-- exact-key rows.  Shape rows stay compact and never become an attribute
-- manifest that must be loaded merely to select the shape itself.
data ShapeInfo = ShapeInfo
  { shapeName        :: TopKey
  , shapeKind        :: ShapeKind
  , shapeLineage     :: [TopKey]
  , shapeConstructor :: Maybe (TopKey, ConstructorDecl)
  , shapeAbstracts   :: [MemberRef]
  } deriving (Eq, Show, Generic)

instance NFData ShapeInfo
instance Persist.Persist ShapeInfo

data SlotInfo = SlotInfo
  { slotProvider :: TopKey
  , slotDecl     :: SlotDecl
  } deriving (Eq, Show, Generic)

instance NFData SlotInfo
instance Persist.Persist SlotInfo

newtype ReflectableAttrs = ReflectableAttrs { reflectableAttrs :: [A.Name] }
                           deriving (Eq, Show, Generic)

instance NFData ReflectableAttrs
instance Persist.Persist ReflectableAttrs


-- In-memory preparation result -------------------------------------------------------------------------

data ReachabilityRows = ReachabilityRows
  { reachModuleSummary   :: ReachSummary
  , reachWholeSummary    :: ReachSummary
  , reachTopRows         :: Map.Map TopKey TopInfo
  , reachMemberRows      :: Map.Map (TopKey, Rows.MemberKey) MemberInfo
  , reachShapeRows       :: Map.Map TopKey ShapeInfo
  , reachSlotRows        :: Map.Map (TopKey, MemberRef) SlotInfo
  , reachReflectableRows :: Map.Map TopKey ReflectableAttrs
  } deriving (Eq, Show, Generic)

instance NFData ReachabilityRows

emptyReachabilityRows :: ReachabilityRows
emptyReachabilityRows = ReachabilityRows
  mempty mempty Map.empty Map.empty Map.empty Map.empty Map.empty
