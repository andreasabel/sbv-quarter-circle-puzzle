-- | Check whether the country sizes is the declared one.

module Valuation where

import Data.SBV

import Val
import Board

-- type SVal = SBV Val -- hits issue #698
type SVal = (SInteger, SInteger) -- workaround

puzzle :: Puzzle
puzzle =
  [ [4, 4]
  , [4, 4]
  ]

test = satWith z3{ verbose = True } . solvable

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
validSquare b (Square i _ _ _) = \case
  (0, 0) -> sTrue
  v -> litVal v  .== boardVal i b

-- | The board area assigned to a color.
--
boardVal :: Color -> Board -> SVal
boardVal i = sumVals . concat . map (map (squareVal i))

-- | The area a square assigns to the given color.
--
-- Can be nothing, a unit square, a quarter circle,
-- or the rest of a unit square if you take away a quarter circle.
--
squareVal :: Color -> Square -> SVal
squareVal i (Square l s _ _) =
  ite (l .== i)
    (ite (s .== i) (litVal 1) (litVal π¼))
    (ite (s .== i) (litVal π¼ᵒᵖ) (litVal 0))

litVal :: Val -> SVal
litVal (x, y) = (literal x, literal y)



-- | This should be just 'sum',
-- but we have to work around <https://github.com/LeventErkok/sbv/issues/698>.
sumVals :: [SVal] -> SVal
sumVals vs = (sum (map fst vs), sum (map snd vs))

-- toVal :: (SInteger, SInteger) -> SBV Val --?
