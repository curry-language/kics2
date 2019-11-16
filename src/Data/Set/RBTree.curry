----------------------------------------------------------------------------
--- Library with an implementation of sets as red-black trees.
---
--- All the operations on sets are generic, i.e., one has to provide
--- an explicit order predicate `(<)` (less-than) on elements.
---
--- @author Johannes Koj, Michael Hanus, Bernd Brassel
--- @version December 2018
----------------------------------------------------------------------------

module Data.Set.RBTree where

import qualified Data.RedBlackTree as RBT
import           Data.Maybe               (isJust)
import           Prelude           hiding (empty)

type SetRBT a = RBT.RedBlackTree a

--- Returns an empty set, i.e., an empty red-black tree
--- augmented with an order predicate.
empty :: Eq a => (a -> a -> Bool) -> SetRBT a
empty = RBT.empty (==) (==)

--- Test for an empty set.
null :: SetRBT _ -> Bool
null = RBT.isEmpty

--- Returns true if an element is contained in a (red-black tree) set.
--- @param e - an element to be checked for containment
--- @param s - a set (represented as a red-black tree)
--- @return True if e is contained in s
member :: a -> SetRBT a -> Bool
member e = isJust . (RBT.lookup e)

--- Inserts an element into a set if it is not already there.
insert :: a -> SetRBT a -> SetRBT a
insert = RBT.update

--- Inserts an element into a multiset.
--- Thus, the same element can have several occurrences in the multiset.
insertMulti :: Eq a => a -> SetRBT a -> SetRBT a
insertMulti e = RBT.setInsertEquivalence (==)
              . RBT.update e
              . RBT.setInsertEquivalence (\ _ _ -> False)

--- delete an element from a set.
--- Deletes only a single element from a multi set
delete :: a -> SetRBT a -> SetRBT a
delete = RBT.delete

--- Transforms a (red-black tree) set into an ordered list of its elements.
toList :: SetRBT a -> [a]
toList = RBT.toList

--- Computes the union of two (red-black tree) sets.
--- This is done by inserting all elements of the first set into the
--- second set.
union :: SetRBT a -> SetRBT a -> SetRBT a
union s1 s2 = foldr insert s2 (toList s1)

--- Computes the intersection of two (red-black tree) sets.
--- This is done by inserting all elements of the first set
--- contained in the second set into a new set, which order
--- is taken from the first set.
intersection :: SetRBT a -> SetRBT a -> SetRBT a
intersection s1 s2 = foldr insert (RBT.newTreeLike s1)
                     (filter (`member` s2) (toList s1))

--- Generic sort based on insertion into red-black trees.
--- The first argument is the order for the elements.
sortBy  :: Eq a => (a -> a -> Bool) -> [a] -> [a]
sortBy = RBT.sortBy
