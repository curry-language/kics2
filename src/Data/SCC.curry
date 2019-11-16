--- ----------------------------------------------------------------------------
--- Computing strongly connected components
---
--- Copyright (c) 2000 - 2003, Wolfgang Lux
--- See LICENSE for the full license.
---
--- The function `scc` computes the strongly connected components of a list
--- of entities in two steps. First, the list is topologically sorted
--- "downwards" using the *defines* relation.
--- Then the resulting list is sorted "upwards" using the *uses* relation
--- and partitioned into the connected components. Both relations
--- are computed within this module using the bound and free names of each
--- declaration.
---
--- In order to avoid useless recomputations, the code in the module first
--- decorates the declarations with their bound and free names and a
--- unique number. The latter is only used to provide a trivial ordering
--- so that the declarations can be used as set elements.
---
--- @author Wolfgang Lux
--- ----------------------------------------------------------------------------

module Data.SCC (scc) where

import Data.Set.RBTree (empty, member, insert)
import Prelude hiding  (empty)

data Node a b = Node Int [b] [b] a
 deriving Eq

cmpNode :: Node a b -> Node a b -> Bool
cmpNode n1 n2 = key n1 < key n2

key :: Node a b -> Int
key (Node k _ _ _) = k

bvs :: Node a b -> [b]
bvs (Node _ bs _ _) = bs

fvs :: Node a b -> [b]
fvs (Node _ _ fs _) = fs

node :: Node a b -> a
node (Node _ _ _ n) = n

--- Computes the strongly connected components of a list
--- of entities. To be flexible, we distinguish the nodes and
--- the entities defined in this node.
---
--- @param defines - maps each node to the entities defined in this node
--- @param uses    - maps each node to the entities used in this node
--- @param nodes   - the list of nodes which should be sorted into
---                  strongly connected components
--- @return the strongly connected components of the list of nodes
scc :: (Eq a, Eq b) =>
       (a -> [b]) -- ^ entities defined by node
    -> (a -> [b]) -- ^ entities used by node
    -> [a]        -- ^ list of nodes
    -> [[a]]      -- ^ strongly connected components
scc bvs' fvs' = map (map node) . tsort' . tsort . zipWith wrap [0 ..]
  where wrap i n = Node i (bvs' n) (fvs' n) n

tsort :: (Eq a, Eq b) => [Node a b] -> [Node a b]
tsort xs = snd (dfs xs (empty cmpNode) [])
  where
  dfs []        marks stack = (marks, stack)
  dfs (x : xs') marks stack
    | x `member` marks = dfs xs' marks stack
    | otherwise         = dfs xs' marks' (x : stack')
    where
    (marks', stack') = dfs (defs x) (x `insert` marks) stack
    defs x1          = filter (any (`elem` fvs x1) . bvs) xs

tsort' :: (Eq a, Eq b) => [Node a b] -> [[Node a b]]
tsort' xs = snd (dfs xs (empty cmpNode) [])
  where
  dfs []        marks stack = (marks, stack)
  dfs (x : xs') marks stack
    | x `member` marks = dfs xs' marks stack
    | otherwise         = dfs xs' marks' ((x : concat stack') : stack)
    where
    (marks', stack') = dfs (uses x) (x `insert` marks) []
    uses x1          = filter (any (`elem` bvs x1) . fvs) xs
