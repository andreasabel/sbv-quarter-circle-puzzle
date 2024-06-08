module Util (module Util, module X) where

import Data.Function      as X     (on)
import Data.List          as X     (transpose)
import Data.List.NonEmpty qualified as List1
import Data.SBV

-- * Binary quantifiers.

-- | Lift binary relation conjunctively to lists.
allZip :: (a -> a -> SBool) -> [a] -> [a] -> SBool
allZip rel xs ys = sAnd $ zipWith rel xs ys

-- | Test all adjacent squares in a row.
allAdjacent :: (a -> a -> SBool) -> [a] -> SBool
allAdjacent rel as = allZip rel as (drop 1 as)

-- | Test all distinct pairs, or more precicely, all subsets of size 2 (choose 2).
-- The relation should be symmetric.
allPairs :: (a -> a -> SBool) -> [a] -> SBool
allPairs rel as = sAll (allZip rel as) (List1.tail (List1.tails as))
