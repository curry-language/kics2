module Data.Set
  ( Set, null, size, fromList, empty, insert, member, delete
  , union, toList, difference
  ) where

import qualified Data.Map as Map

-------------------------------------------------------------------------
--                                                                      -
--   FiniteSets---a thin veneer                                         -
--                                                                      -
-------------------------------------------------------------------------
type Set key = Map.Map key ()

empty :: Set key
empty = Map.empty

fromList :: Ord key => [key] -> Set key
fromList xs = Map.fromList [ (x, ()) | x <- xs]

null :: Set key -> Bool
null = Map.null

insert :: Ord key => key -> Set key -> Set key
insert k s = Map.insert k () s

delete :: Ord key => key -> Set key -> Set key
delete k s = Map.delete k s

size :: Set key -> Int
size = Map.size

member :: Ord key => key -> Set key -> Bool
member = Map.member

difference :: Ord key => Set key -> Set key -> Set key
difference = Map.difference

toList :: Set key -> [key]
toList = Map.keys

union :: Ord key => Set key -> Set key -> Set key
union = Map.union
