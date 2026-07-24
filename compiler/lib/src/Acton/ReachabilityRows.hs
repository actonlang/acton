-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE DeriveGeneric #-}
module Acton.ReachabilityRows
  ( TopKey(..)
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
import Acton.ReachabilityTypes
import qualified Acton.Syntax as A

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import qualified Data.Persist as Persist
import GHC.Generics (Generic)


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
