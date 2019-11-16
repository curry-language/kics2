-----------------------------------------------------------------------------
--- A finite map is an efficient purely functional data structure
--- to store a mapping from keys to values.
---
--- This version was ported from a corresponding Haskell library
---
--- @author Frank Huch, Bernd Brassel
--- @version March 2013
--- @category algorithm
-----------------------------------------------------------------------------

module Data.Map (
        Map,                -- abstract type

        empty,
        singleton,
        fromList,

        insert,
        insertWith,
        insertList,
        insertListWith,
        delete,
        deleteAll,
        adjust,
        splitLookup,

        union,
        unionWith,
        difference,
        intersection,
        intersectionWith,

        foldrWithKey,
        mapWithKey,
        filterWithKey,

        size,
        null,
        member,
        lookup,
        findWithDefault,

        toList,
        keys,
        elems,
        sortWithMap,

        lookupMin,
        lookupMax,
        toPreOrderList

    ) where

import Data.Maybe
import Prelude hiding (empty)

-----------------------------------------------
--        BUILDING finite maps
-----------------------------------------------

--- The empty map.
--- @result an empty map
empty :: Map _ _
empty = Tip

--- Construct a map with only a single element.
--- @param k key of
--- @param a the single element to form
--- @result a finite map with only a single element
singleton :: k -> a -> Map k a
singleton k a = Bin k a 1 Tip Tip


--- Builts a map from given list of tuples (key,element).
--- For multiple occurences of key, the last corresponding
--- element of the list is taken.
fromList :: Ord k => [(k,a)] -> Map k a
fromList xs = insertList xs empty

-----------------------------------------------
--        ADDING AND DELETING
-----------------------------------------------

--- Throws away any previous binding and stores the new one given.
insert :: Ord k => k -> a -> Map k a -> Map k a
insert k a m = insertWith (\ _ new -> new) k a m

--- Instead of throwing away the old binding,
--- insertWith combines the new element with the old one.
--- @param combiner a function combining to elements
--- @param k the key of the elements to be combined
--- @param a the new element
--- @param m a map
--- @result a modified map
insertWith :: Ord k => (a -> a -> a)
           -> k -> a -> Map k a -> Map k a
insertWith _ k a Tip = singleton k a
insertWith combiner new_k new_a (Bin k a sizeM m_l m_r)
  = if new_k < k
    then mkBalBranch k a (insertWith combiner new_k new_a m_l) m_r
    else
      if new_k == k
      then Bin new_k (combiner a new_a) sizeM m_l m_r
      else mkBalBranch k a m_l (insertWith combiner new_k new_a m_r)


--- Throws away any previous bindings and stores the new ones given.
--- The items are added starting with the first one in the list
insertList :: Ord k => [(k, a)] -> Map k a -> Map k a
insertList k_a_pairs m = insertListWith (\ _ new -> new) k_a_pairs m

--- Combine with a list of tuples (key, element), cf. insertWith
insertListWith :: Ord k => (a -> a -> a)
               -> [(k, a)] -> Map k a -> Map k a
insertListWith combiner k_a_pairs m
  = foldl add m k_a_pairs        -- foldl adds from the left
  where
    add m' (k, a) = insertWith combiner k a m'

--- Deletes key from map.
--- Deletion doesn't complain if you try to delete something
--- which isn't there
delete :: Ord k => k -> Map k a -> Map k a
delete _ Tip = Tip
delete del_k (Bin k a _ m_l m_r)
  = if del_k < k
    then mkBalBranch k a (delete del_k m_l) m_r
    else
      if del_k == k
        then glueBal m_l m_r
        else mkBalBranch k a m_l (delete del_k m_r)

--- Deletes a list of keys from map.
--- Deletion doesn't complain if you try to delete something
--- which isn't there
deleteAll :: Ord k => [k] -> Map k a -> Map k a
deleteAll ks m = foldl (flip delete) m ks

--- Applies a function to element bound to given key.
adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust _ _ Tip          = Tip
adjust f i (Bin k x h l r)
            | i == k    =  Bin k (f x) h l r
            | i <  k    =  Bin k x h (adjust f i l) r
            | otherwise =  Bin k x h l (adjust f i r)

--- Combines delFrom and lookup.
splitLookup :: Ord k => k -> Map k a -> (Map k a, Maybe a, Map k a)
splitLookup _ Tip = (Tip, Nothing, Tip)
splitLookup i (Bin k a _ m_l m_r)
            | i == k    = (m_l, Just a, m_r)
            | i <  k    = let (m_l', v, m_r') = splitLookup i m_l
                          in (m_l', v, glueBal m_r' m_r)
            | otherwise = let (m_l', v, m_r') = splitLookup i m_r
                          in (glueBal m_l m_l', v, m_r')

-------------------------------------------------
-- COMBINING finite maps
-------------------------------------------------


--- Efficiently add key/element mappings of two maps into a single one.
--- CHANGED: Bindings in left argument shadow those in the right
union :: Ord k => Map k a -> Map k a -> Map k a
union  Tip m2 = m2
union  m1@(Bin _ _ _ _ _) Tip = m1
union  (Bin split_key1 a1 _ left1 right1) m2@(Bin _ _ _ _ _)
  = mkVBalBranch split_key1 a1 (union left1 lts) (union right1 gts)
  where
    lts     = splitLT m2 split_key1
    gts     = splitGT m2 split_key1

--- Efficiently combine key/element mappings of two maps into a single one,
--- cf. insertWith
unionWith :: Ord k => (a -> a -> a)
          -> Map k a -> Map k a -> Map k a
unionWith _        Tip m2 = m2
unionWith _        m1@(Bin _ _ _ _ _) Tip = m1
unionWith combiner (Bin split_key1 a1 _ left1 right1) m2@(Bin _ _ _ _ _)
  = mkVBalBranch split_key1 new_a
                 (unionWith combiner left1 lts)
                 (unionWith combiner right1 gts)
  where
    lts   = splitLT m2 split_key1
    gts   = splitGT m2 split_key1
    new_a = case lookup split_key1 m2 of
              Nothing -> a1
              Just a2 -> combiner a2 a1

--- (difference a1 a2) deletes from a1 any bindings which are bound in a2
difference :: Ord k => Map k a -> Map k b -> Map k a
difference Tip _ = Tip
difference m1@(Bin _ _ _ _ _) Tip = m1
difference m1@(Bin _ _ _ _ _) (Bin split_key2 _ _ left2 right2)
  = glueVBal (difference lts left2) (difference gts right2)
       -- The two can be way different, so we need glueVBal
  where
    lts = splitLT m1 split_key2  -- NB gt and lt, so the equal ones
    gts = splitGT m1 split_key2  -- are not in either.

--- Filters only those keys that are bound in both of the given maps.
--- CHANGED: The elements will be taken from the first map.
intersection :: Ord k => Map k a -> Map k a -> Map k a
intersection m1 m2 = intersectionWith (\ left _ -> left) m1 m2

--- Filters only those keys that are bound in both of the given maps
--- and combines the elements as in insertWith.
intersectionWith :: Ord k => (a -> b -> c)
                 -> Map k a -> Map k b -> Map k c
intersectionWith _        _   Tip             = Tip
intersectionWith _        Tip (Bin _ _ _ _ _) = Tip
intersectionWith combiner m1@(Bin _ _ _ _ _) (Bin split_key2 a2 _  left2 right2)

  | isJust maybe_a1   -- split_a *is* in intersection
  = mkVBalBranch split_key2 (combiner a1' a2)
                 (intersectionWith combiner lts left2)
                 (intersectionWith combiner gts right2)

  | otherwise           -- split_a is *not* in intersection
  = glueVBal (intersectionWith combiner lts left2)
             (intersectionWith combiner gts right2)

  where
    lts = splitLT m1 split_key2      -- NB gt and lt, so the equal ones
    gts = splitGT m1 split_key2      -- are not in either.

    maybe_a1 = lookup split_key2 m1
    Just a1'  = maybe_a1

-------------------------------------------------------------
--  MAPPING, FOLDING, FILTERING on maps
-------------------------------------------------------------

--- Folds map by given function.
foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey _ z Tip = z
foldrWithKey k z (Bin key elt _ fm_l fm_r)
  = foldrWithKey k (k key elt (foldrWithKey k z fm_r)) fm_l

--- Applies a given function on every element in the map.
mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey _ Tip = Tip
mapWithKey f (Bin key a size' m_l m_r)
  = Bin key (f key a) size' (mapWithKey f m_l) (mapWithKey f m_r)

--- Yields a new map with only those key/element pairs matching the
--- given predicate.
filterWithKey :: Ord k => (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey _ Tip = Tip
filterWithKey p (Bin key a _ m_l m_r)
  | p key a          -- Keep the item
  = mkVBalBranch key a (filterWithKey p m_l) (filterWithKey p m_r)

  | otherwise          -- Drop the item
  = glueVBal (filterWithKey p m_l) (filterWithKey p m_r)

-----------------------------------------------------
-- INTERROGATING maps
-----------------------------------------------------

--- How many elements does given map contain?
size :: Map _ _ -> Int
size Tip                 = 0
size (Bin _ _ size' _ _) = size'

--- Is the given finite map empty?
null :: Map _ _ -> Bool
null m = size m == 0

--- Does given map contain given key?
member :: Ord k => k -> Map k _ -> Bool
member k m = isJust (lookup k m)

--- Retrieves element bound to given key
lookup :: Ord k => k -> Map k a -> Maybe a
lookup _ Tip = Nothing
lookup key_to_find (Bin k a _ m_l m_r)
  = if key_to_find < k
    then lookup key_to_find m_l
    else if key_to_find == k
         then Just a
         else lookup key_to_find m_r

--- Retrieves element bound to given key.
--- If the element is not contained in map, return
--- default value.
findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault deflt k m
  = case lookup k m of
      Nothing -> deflt
      Just a  -> a

--- Retrieves the smallest key/element pair in the finite map
--- according to the basic key ordering.
lookupMin :: Map k a -> Maybe (k, a)
lookupMin  Tip                         = Nothing
lookupMin (Bin k x _ l _) | isBranch l = lookupMin l
                          | otherwise  = Just (k, x)

--- Retrieves the greatest key/element pair in the finite map
--- according to the basic key ordering.
lookupMax :: Map k a -> Maybe (k, a)
lookupMax Tip                          = Nothing
lookupMax (Bin k x _ _ r) | isBranch r = lookupMax r
                          | otherwise  = Just (k, x)



----------------------------------------------------
-- LISTIFYING: transform finite maps to lists
----------------------------------------------------

--- Builds a list of key/element pairs. The list is ordered
--- by the "Ord" context on keys.
toList :: Map k a -> [(k, a)]
toList m = foldrWithKey (\ k a rest -> (k, a) : rest) [] m

--- Retrieves a list of keys contained in map.
--- The list is ordered
--- by the "Ord" context on keys.
keys :: Map k _ -> [k]
keys m = foldrWithKey (\ k _ rest -> k : rest) [] m

--- Retrieves a list of elements contained in map.
--- The list is ordered
--- by the "Ord" context on keys.
elems :: Map _ a -> [a]
elems m = foldrWithKey (\ _ a rest -> a : rest) [] m

--- Retrieves list of key/element pairs in preorder of the internal tree.
--- Useful for lists that will be retransformed into a tree or to match
--- any elements regardless of basic order.
toPreOrderList :: Map k a -> [(k, a)]
toPreOrderList m = pre m []
   where
     pre Tip xs = xs
     pre (Bin k x _ l r) xs = (k, x) : pre l (pre r xs)

--- Sorts a given list by inserting and retrieving from map.
--- Duplicates are deleted.
sortWithMap :: Ord k => [k] -> [k]
sortWithMap l = keys (fromList (zip l (repeat ())))

-----------------------------------------------------
-- internal Implementation
-----------------------------------------------------
data Map k a
  = Tip
  | Bin k a -- Key and element stored here
    Int{-STRICT-}    -- Size >= 1
    (Map k a)        -- Children
    (Map k a)
  deriving (Show, Read)

instance (Eq k, Eq a) => Eq (Map k a) where
  m_1 == m_2 =
    (size   m_1 == size   m_2) &&   -- quick test
    (toList m_1 == toList m_2)

isBranch :: Map _ _ -> Bool
isBranch (Bin _ _ _ _ _) = True
isBranch Tip             = False

-------------------------------------------------------------------------
--                                                                      -
--  The implementation of balancing                                     -
--                                                                      -
-------------------------------------------------------------------------
-------------------------------------------------------------------------
--                                                                      -
--  Basic construction of a FiniteMap                                   -
--                                                                      -
-------------------------------------------------------------------------
sIZE_RATIO :: Int
sIZE_RATIO = 5

mkBranch :: Int
         -> key -> elt
         -> Map key elt -> Map key elt
         -> Map key elt

mkBranch _{-which-} key elt fm_l fm_r =
    let result = Bin key elt (unbox (1 + left_size + right_size)) fm_l fm_r
    in
      result
      --    if size result <= 8 then
      --     result
      --    else
      --      pprTrace ("mkBranch:"++(show which)) (ppr result) (
      --      result
      --      )
  where
    {-left_ok  = case fm_l of
                 Tip            -> True
                 Bin _ _ _ _ _  -> cmpWithBiggest_left_key key

    cmpWithBiggest_left_key key' = (fst (findMax fm_l)) < key'

    right_ok = case fm_r of
                 Tip           -> True
                 Bin _ _ _ _ _ -> cmpWithSmallest_right_key key

    cmpWithSmallest_right_key key' = key' < (fst (findMin fm_r))

    balance_ok = True -- sigh-}
    left_size  = size fm_l
    right_size = size fm_r


    unbox :: Int -> Int
    unbox x = x


-------------------------------------------------------------------------
--                                                                        -
-- Balanced construction of a FiniteMap                                 -
--                                                                        -
-------------------------------------------------------------------------
mkBalBranch :: key -> elt
            -> Map key elt -> Map key elt
            -> Map key elt

mkBalBranch key elt fm_L fm_R

  | size_l + size_r < 2
  = mkBranch 1{-which-} key elt fm_L fm_R

  | size_r > sIZE_RATIO * size_l        -- Right tree too big
  = case fm_R of
        Bin _ _ _ fm_rl fm_rr ->
              if size fm_rl < 2 * size fm_rr
                then single_L fm_L fm_R
                else double_L fm_L fm_R
        -- Other case impossible
        Tip -> error "Data.Map.mkBalBranch"

  | size_l > sIZE_RATIO * size_r        -- Left tree too big
  = case fm_L of
        Bin _ _ _ fm_ll fm_lr ->
              if size fm_lr < 2 * size fm_ll
                then single_R fm_L fm_R
                else double_R fm_L fm_R
        -- Other case impossible
        Tip -> error "Data.Map.mkBalBranch"

  | otherwise                                -- No imbalance
  = mkBranch 2{-which-} key elt fm_L fm_R

  where
    size_l   = size fm_L
    size_r   = size fm_R

    single_L fm_l (Bin key_r elt_r _ fm_rl fm_rr)
        = mkBranch 3{-which-} key_r elt_r (mkBranch 4{-which-} key elt fm_l fm_rl) fm_rr
    single_L _ Tip = error "Data.Map.single_L"

    double_L fm_l (Bin key_r elt_r _ (Bin key_rl elt_rl _ fm_rll fm_rlr) fm_rr)
        = mkBranch 5{-which-} key_rl elt_rl (mkBranch 6{-which-} key   elt   fm_l   fm_rll)
                                 (mkBranch 7{-which-} key_r elt_r fm_rlr fm_rr)
    double_L _ Tip = error "Data.Map.double_L"
    double_L _ (Bin _ _ _ Tip _) = error "Data.Map.double_L"

    single_R (Bin key_l elt_l _ fm_ll fm_lr) fm_r
        = mkBranch 8{-which-} key_l elt_l fm_ll (mkBranch 9{-which-} key elt fm_lr fm_r)
    single_R Tip _ = error "Data.Map.single_R"

    double_R (Bin key_l elt_l _ fm_ll (Bin key_lr elt_lr _ fm_lrl fm_lrr)) fm_r
        = mkBranch 10{-which-} key_lr elt_lr (mkBranch 11{-which-} key_l elt_l fm_ll  fm_lrl)
                                 (mkBranch 12{-which-} key   elt   fm_lrr fm_r)
    double_R Tip _ = error "Data.Map.double_R"
    double_R (Bin _ _ _ _ Tip) _ = error "Data.Map.double_R"


mkVBalBranch :: Ord key => key -> elt -> Map key elt
             -> Map key elt -> Map key elt

-- Assert: in any call to (mkVBalBranch_C comb key elt l r),
--           (a) all keys in l are < all keys in r
--           (b) all keys in l are < key
--           (c) all keys in r are > key

mkVBalBranch key elt Tip fm_r = insert key elt fm_r
mkVBalBranch key elt (Bin key_l elt_l s_l fm_ll fm_lr) Tip =
   insert key elt (Bin key_l elt_l s_l fm_ll fm_lr)

mkVBalBranch key elt (Bin key_l elt_l s_l fm_ll fm_lr)
                     (Bin key_r elt_r s_r fm_rl fm_rr)
  | sIZE_RATIO * size_l < size_r
  = mkBalBranch key_r elt_r (mkVBalBranch key elt fm_l fm_rl) fm_rr

  | sIZE_RATIO * size_r < size_l
  = mkBalBranch key_l elt_l fm_ll (mkVBalBranch key elt fm_lr fm_r)

  | otherwise
  = mkBranch 13{-which-} key elt fm_l fm_r

  where
    fm_l = Bin key_l elt_l s_l fm_ll fm_lr
    fm_r = Bin key_r elt_r s_r fm_rl fm_rr
    size_l = size fm_l
    size_r = size fm_r

-------------------------------------------------------------------------
--                                                                        -
-- Gluing two trees together                                            -
--                                                                        -
-------------------------------------------------------------------------
glueBal :: Map key elt -> Map key elt -> Map key elt
glueBal fm1 fm2 =
  if null fm1
    then fm2
    else if null fm2
           then fm1
           else
        -- The case analysis here (absent in Adams' program) is really to deal
        -- with the case where fm2 is a singleton. Then deleting the minimum means
        -- we pass an empty tree to mkBalBranch, which breaks its invariant.
             let (mid_key1, mid_elt1) = findMax fm1
                 (mid_key2, mid_elt2) = findMin fm2
             in
             if size fm2 > size fm1
               then mkBalBranch mid_key2 mid_elt2 fm1 (deleteMin fm2)
               else mkBalBranch mid_key1 mid_elt1 (deleteMax fm1) fm2

glueVBal :: Map key elt -> Map key elt -> Map key elt
glueVBal fm_l fm_r =
  if null fm_l
    then fm_r
    else if null fm_r
           then fm_l
           else
             let Bin key_l elt_l _ fm_ll fm_lr = fm_l
                 Bin key_r elt_r _ fm_rl fm_rr = fm_r
                 --(mid_key_l,mid_elt_l) = findMax fm_l
                 --(mid_key_r,mid_elt_r) = findMin fm_r
                 size_l = size fm_l
                 size_r = size fm_r
             in
               if sIZE_RATIO * size_l < size_r
               then
                 mkBalBranch key_r elt_r (glueVBal fm_l fm_rl) fm_rr
                else if sIZE_RATIO * size_r < size_l
                    then
                      mkBalBranch key_l elt_l fm_ll (glueVBal fm_lr fm_r)

                      -- We now need the same two cases as in glueBal above.
                    else glueBal fm_l fm_r

-------------------------------------------------------------------------
--                                                                        -
-- Local utilities                                                      -
--                                                                        -
-------------------------------------------------------------------------

splitLT, splitGT :: Ord key => Map key elt -> key -> Map key elt

-- splitLT fm split_key  =  fm restricted to keys <  split_key
-- splitGT fm split_key  =  fm restricted to keys >  split_key

splitLT  Tip _ = Tip
splitLT (Bin key elt _ fm_l fm_r) split_key
  = if split_key < key
    then splitLT fm_l split_key
    else if split_key == key
         then fm_l
         else mkVBalBranch key elt fm_l (splitLT fm_r split_key)

splitGT Tip _ = Tip
splitGT (Bin key elt _ fm_l fm_r) split_key
  = if split_key < key
    then mkVBalBranch key elt (splitGT fm_l split_key) fm_r
    else if split_key == key
         then fm_r
         else splitGT fm_r split_key

findMin :: Map key elt -> (key, elt)
findMin Tip = error "Data.Map.findMin: empty map"
findMin (Bin key elt _ Tip _) = (key, elt)
findMin (Bin _   _   _ (Bin key_l elt_l s_l fm_ll fm_lr)_) =
      findMin (Bin key_l elt_l s_l fm_ll fm_lr)

deleteMin :: Map key elt -> Map key elt
deleteMin Tip                      = error "Data.Map.deleteMin: empty map"
deleteMin (Bin _   _   _ Tip fm_r) = fm_r
deleteMin (Bin key elt _ (Bin key_l elt_l s_l fm_ll fm_lr) fm_r) =
  mkBalBranch key elt (deleteMin (Bin key_l elt_l s_l fm_ll fm_lr))
                         fm_r

findMax :: Map key elt -> (key,elt)
findMax Tip = error "Data.Map.findMax: empty map"
findMax (Bin key elt _ _ Tip) = (key, elt)
findMax (Bin _   _   _ _  (Bin key_r elt_r s_r fm_rl fm_rr)) =
  findMax (Bin key_r elt_r s_r fm_rl fm_rr)

deleteMax :: Map key elt -> Map key elt
deleteMax Tip                      = error "FiniteMap.deleteMax: empty map"
deleteMax (Bin _   _   _ fm_l Tip) = fm_l
deleteMax (Bin key elt _ fm_l (Bin key_r elt_r s_r fm_rl fm_rr)) =
  mkBalBranch key elt fm_l
              (deleteMax (Bin key_r elt_r s_r fm_rl fm_rr))
