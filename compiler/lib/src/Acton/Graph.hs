-- | Directed graphs over ordered keys: reachability in both directions,
-- cycles, and the nodes on paths between two sets of nodes.
module Acton.Graph
  ( Graph, nodes, graph, depsOf, usersOf
  , closure, dependents, strictDependents, cycles, pathsTo
  ) where

import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Containers.ListUtils (nubOrd)
import Data.Graph (SCC(..), stronglyConnComp)

-- | A directed graph whose edges point from a node to the nodes it depends
-- on.  Edges to nodes outside the graph are dropped when it is built, and a
-- node's dependencies keep their first-occurrence order.  The order of a
-- node's users carries no meaning: every use takes a set or a maximum.
data Graph k = Graph
  { nodes        :: [k]                    -- in the order the graph was built from
  , dependencies :: M.Map k [k]
  , users        :: M.Map k [k]
  }

graph                                   :: Ord k => [(k, [k])] -> Graph k
graph adjacency                         = Graph (map fst adjacency) deps reverseDeps
  where known                           = Set.fromList (map fst adjacency)
        deps                            = M.fromList [ (k, nubOrd (filter (`Set.member` known) ds)) | (k, ds) <- adjacency ]
        reverseDeps                     = M.fromListWith (++) [ (d, [k]) | (k, ds) <- M.toList deps, d <- ds ]

depsOf, usersOf                         :: Ord k => Graph k -> k -> [k]
depsOf g k                              = M.findWithDefault [] k (dependencies g)
usersOf g k                             = M.findWithDefault [] k (users g)

-- | Every node reachable from the start nodes by `next`, start nodes included.
closure                                 :: Ord k => (k -> [k]) -> [k] -> Set.Set k
closure next                            = go Set.empty
  where go seen []                      = seen
        go seen (k : ks)
          | Set.member k seen           = go seen ks
          | otherwise                   = go (Set.insert k seen) (next k ++ ks)

-- | The start nodes and everything that depends on them, directly or not.
dependents                              :: Ord k => Graph k -> [k] -> Set.Set k
dependents g                            = closure (usersOf g)

-- | Everything that depends on a node, directly or not; the node itself only on a cycle.
strictDependents                        :: Ord k => Graph k -> k -> Set.Set k
strictDependents g k                    = dependents g (usersOf g k)

-- | The cycles, as stronglyConnComp reports them for the nodes in their given order.
cycles                                  :: Ord k => Graph k -> [[k]]
cycles g                                = [ ks | CyclicSCC ks <- stronglyConnComp [ (k, k, depsOf g k) | k <- nodes g ] ]

-- | The nodes on dependency paths from the start nodes to a target: reachable
-- from a start node, start nodes included, and leading to a target.  Safe on cycles.
pathsTo                                 :: Ord k => Graph k -> (k -> Bool) -> [k] -> Set.Set k
pathsTo g isTarget start                = closure (depsOf g) start `Set.intersection` dependents g (filter isTarget (nodes g))
