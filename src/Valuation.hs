-- | Check whether the country sizes is the declared one.

module Valuation where

import Data.SBV

import Val
import Board

type SVal = SBV Val

puzzle :: Puzzle
puzzle =
  [ [4, 4]
  , [4, 4]
  ]

solvable :: Puzzle -> Symbolic SBool
solvable p = validSolution p <$> mkBoard p

validSolution :: Puzzle -> Board -> SBool
validSolution p b = validColoring b .&& validValuation b p

-- | Does a well-formed board coloring solve the puzzle?
--
-- When a square of color @i@ is labeled with value @v@ in the puzzle,
-- the board area of this color should match the value.
validValuation :: Board -> Puzzle -> SBool
validValuation b p = sAnd $ concat $ zipWith (zipWith (validSquare b)) b p

validSquare :: Board -> Square -> Val -> SBool
validSquare b (Square i _ _ _) v0 = v .== 0 .|| v .== boardVal i b
  where v = literal v0

-- | The board area assigned to a color.
--
boardVal :: Color -> Board -> SBV Val
boardVal i = sum . concat . map (map (squareVal i))

-- | The area a square assigns to the given color.
--
-- Can be nothing, a unit square, a quarter circle,
-- or the rest of a unit square if you take away a quarter circle.
--
squareVal :: Color -> Square -> SBV Val
squareVal i (Square l s _ _) =
  ite (l .== i)
    (ite (s .== i) 1 (literal π¼))
    (ite (s .== i) (literal π¼ᵒᵖ) 0)
