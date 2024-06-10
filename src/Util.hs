module Util (module Util, module X) where

import Data.Function      as X     (on)
import Data.List          as X     (transpose)
import Data.List.NonEmpty qualified as List1
import Data.SBV

-- * Binary quantifiers.

-- | Lift binary relation conjunctively to lists.
--
-- @
--    allZip rel [a 0, a 1, ..., a n]
--               [b 0, b 1, ..., b m]
--    =
--          a 0 `rel` b 0
--      .&& a 1 `rel` b 1
--      .&& ...
--      .&& a (min n m) `rel` b (min n m)
-- @
--
allZip :: (a -> a -> SBool) -> [a] -> [a] -> SBool
allZip rel xs ys = sAnd $ zipWith rel xs ys

-- | Test all adjacent squares in a row.
--
-- @
--    allAdjacent rel [a 0, a 1, ..., a n]
--    =
--          a 0 `rel` a 1
--      .&& a 1 `rel` a 2
--      .&& ...
--      .&& a (n - 1) `rel` a n
-- @
--
allAdjacent :: (a -> a -> SBool) -> [a] -> SBool
allAdjacent rel as = allZip rel as (drop 1 as)

-- | Test all distinct pairs, or more precicely, all subsets of size 2 (choose 2).
-- The relation should be symmetric.
--
-- @
--    allPairs rel [a 0, a 1, ..., a n]
--    =
--          a 0 `rel` a 1       .&& a 1 `rel` a 2 .&& a 2 `rel` a 3 .&& ... .&& a (n - 1) `rel` a n
--      .&& a 0 `rel` a 2       .&& a 1 `rel` a 3 .&& ... .&& a (n - 2) `rel` a n
--      .&& ...
--      .&& a 0 `rel` a (n - 1) .&& a 1 `rel` a n
--      .&& a 0 `rel` a n
-- @
--
allPairs :: (a -> a -> SBool) -> [a] -> SBool
allPairs rel as = sAll (allZip rel as) (List1.tail (List1.tails as))

-- | Apply a predicate to all squares (2 column vectors of size 2).
--
-- @
--    allSquares rel
--      [ [ a 0 0, a 0 1, a 0 2, ...., a 0 (m 0) ]
--      , [ a 1 0, a 1 1, a 1 2, ...., a 1 (m 1) ]
--      , [ a 2 0, a 2 1, a 2 2, ...., a 2 (m 2) ]
--      , ...
--      , [ a n 0, a n 1, a n 2, ...., a n (m n) ]
--      ]
--    = sAnd
--      [     (a 0 0, a 1 0) `rel` (a 0 1, a 1 1)
--        .&& (a 0 1, a 1 1) `rel` (a 0 2, a 1 2)
--        .&& ...
--        .&& (a 0 (l 0 - 1), a 1 (l 0 - 1) `rel` (a 0 (l 0), a 1 (l 0))
--      ,
--            (a 1 0, a 2 0) `rel` (a 1 1, a 2 1)
--        .&& (a 1 1, a 2 1) `rel` (a 1 2, a 2 2)
--        .&& ...
--        .&& (a 1 (l 1 - 1), a 2 (l 1 - 1) `rel` (a 1 (l 1), a 2 (l 1))
--      ,
--        ...
--      ,
--            (a (n - 1) 0, a n 0) `rel` (a (n - 1) 1, a n 1)
--        .&& (a (n - 1) 1, a n 1) `rel` (a (n - 1) 2, a n 2)
--        .&& ...
--        .&& (a (n - 1) (l (n - 1) - 1), a n (l (n - 1) - 1) `rel` (a (n - 1) (l (n - 1)), a n (l (n - 1)))
--      ]
--      where l i = min (m i) (m (i + 1))
--
allSquares :: ((a, a) -> (a, a) -> SBool) -> [[a]] -> SBool
allSquares rel = allAdjacent \ r1 r2 -> allAdjacent rel $ zip r1 r2

-- * Misc

-- | Either 0 or 2 or more of the 4 conditions should hold.
notExactlyOne :: SBool -> SBool -> SBool -> SBool -> SBool
notExactlyOne a b c d =
  ite a (sOr [b, c, d]) $
  ite b (sOr [c, d]) $
  c .== d

-- | (Row, column) coordinates.
coordinates :: [[(Int, Int)]]
coordinates = map (\ r -> map (r,) [0..]) [0..]
