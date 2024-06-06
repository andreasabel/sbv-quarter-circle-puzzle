-- | The board contains the solution in a form benefitial for SMT
-- plus auxiliary data we need to verify the correctness of the solution.
--
-- A square contains information how the square is divided into two parts
-- and what the colors of the 'large' and 'small' parts are.
-- If the parts have the same color, the square is actually not divided.
-- Otherwise, the bits 'north' and 'west' indicate which of the four
-- sectors N, W, S, E of the square the quarter-circle shaped line
-- passes through.
--
-- The information in the squares partitions the board into areas.
-- The areas should form connected countries.
-- So, each country should have a captial square,
-- and each other square should have a finite path to this capital.
-- We represent these paths locally in a square
-- by a distance to the capital (to rule out infinite paths),
-- and a direction where the capital lies.
-- Going in this direction brings us a step closer to the capital.
--
-- Let us assume the tourist seeking the capital looks SE,
-- then he can step to an adjacent square by being told
-- whether to move horizontally or vertically,
-- and whether to go forward or backward.

module Board where

import Control.Monad
import Data.List (transpose)
import Data.SBV

-- | Info board, containing the solution and other information.
--
type Board = [[Square]]

-- | Coloring maps.  All areas of the same color belong to the same country.
--
type Color = SInteger

-- | A square can have one or two colors.
--
-- In the latter case, it is divided by a quarter circle
-- which can have 4 different orientations, defined by two bits.
--
data Square = Square
  { large   :: Color  -- ^ Coloring of the larger part.
  , small   :: Color  -- ^ Coloring of the smaller part.
  , north   :: SBool  -- ^ If 'True', smaller part is to the north otherwise to the south.
  , west    :: SBool  -- ^ If 'True', smaller part is to the west, else to the east.
  , capital :: SBool -- ^ If 'True', this square is the capital of a country.
  }

-- * Creating a symbolic board
------------------------------------------------------------------------

-- | Create an empty board of the given shape.
--
mkBoard :: [[a]] -> Symbolic Board
mkBoard = zipWithM mkRow [0..]

-- | Create an empty row of the given shape.
--
mkRow :: Int -> [a] -> Symbolic [Square]
mkRow row colShape = zipWithM (\ col _ -> mkSquare row col ) [0..] colShape

-- | Create an empty square at the given coordinates.
--
mkSquare :: Int -> Int -> Symbolic Square
mkSquare row col = Square
    <$> symbolic (name "Large")
    <*> symbolic (name "Small")
    <*> symbolic (name "North")
    <*> symbolic (name "West" )
    <*> symbolic (name "Captial")
  where
    name s = concat [ s, "[", show row, ",", show col, "]" ]

-- * Constraining the board: adjacent squares need to fit
------------------------------------------------------------------------

-- | Do the colors of the squares fit together?
--
validColoring :: Board -> SBool
validColoring b =
  sAll (allAdjacent matchHorizontally) b
  .&&
  sAll (allAdjacent matchVertically) (transpose b)

-- | Test all adjacent squares in a row.
allAdjacent :: (a -> a -> SBool) -> [a] -> SBool
allAdjacent f as = sAnd $ zipWith f as (drop 1 as)

-- | Color of the Northern edge.
northColor :: Square -> Color
northColor (Square l s n _ _) = ite n s l

-- | Color of the Southern edge.
southColor :: Square -> Color
southColor (Square l s n _ _) = ite n l s

-- | Color of the Western edge.
westColor :: Square -> Color
westColor (Square l s _ w _) = ite w s l

-- | Color of the Eastern edge.
eastColor :: Square -> Color
eastColor (Square l s _ w _) = ite w l s

-- | When do two vertically adjacent squares match?
--
-- The south edge of the top square needs to have
-- the same color as the north edge of the bottom square.
matchVertically ::
     Square  -- ^ Top square.
  -> Square  -- ^ Botton square.
  -> SBool
matchVertically t b = southColor t .== northColor b .&& linesConnect t b

-- | When do two horizontally adjacent squares match?
matchHorizontally ::
     Square  -- ^ Left square.
  -> Square  -- ^ Right square.
  -> SBool
matchHorizontally l r = eastColor l .== westColor r .&& linesConnect l r

-- | When do lines of adjacent squares connect?
-- A line crosses two adjacent of the four sectors N, W, S, E of a square.
-- Two lines connect if they share exactly one of these sectors.
-- E.g. NW connects to SW and NE, but not to SE or NW.
--
linesConnect :: Square -> Square -> SBool
linesConnect x y = (north x .== north y) .<+> (west x .== west y)
