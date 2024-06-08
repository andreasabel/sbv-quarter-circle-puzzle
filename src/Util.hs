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

-- | Apply a predicate to all squares (2 column vectors of size 2).
allSquares :: ((a, a) -> (a, a) -> SBool) -> [[a]] -> SBool
allSquares rel = allAdjacent \ r1 r2 -> allAdjacent rel $ zip r1 r2

-- | Either 0 or 2 or more of the 4 conditions should hold.
notExactlyOne :: SBool -> SBool -> SBool -> SBool -> SBool
notExactlyOne a b c d =
  ite a (sOr [b, c, d]) $
  ite b (sOr [c, d]) $
  c .== d

-- | (Row, column) coordinates.
coordinates :: [[(Int, Int)]]
coordinates = map (\ r -> map (r,) [0..]) [0..]
