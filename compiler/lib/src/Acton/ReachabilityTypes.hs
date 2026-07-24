-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE DeriveGeneric #-}
module Acton.ReachabilityTypes
  ( MemberRef(..)
  , ReachEdge(..)
  , ReachSummary
  , reachEdges
  , reachSummaryFromEdges
  , singletonReach
  ) where

import qualified Acton.Syntax as A

import Control.DeepSeq (NFData)
import qualified Data.Persist as Persist
import qualified Data.Set as Set
import GHC.Generics (Generic)


-- These types live below Env and InterfaceFiles so persisted interface rows
-- can use them without introducing a compiler-module import cycle.
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
