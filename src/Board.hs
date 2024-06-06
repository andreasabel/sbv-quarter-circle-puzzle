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
import Data.List (tails, transpose)
import Data.SBV

-- | Info board, containing the solution and other information.
--
type Board = [[Square]]

-- | Coloring maps.  All areas of the same color belong to the same country.
--
-- Negative colors indicate neutral territory.
type Color = SInteger

-- | A square can have one or two colors.
--
-- In the latter case, it is divided by a quarter circle
-- which can have 4 different orientations, defined by two bits.
--
data Square = Square
  { large    :: Color     -- ^ Coloring of the larger part.
  , small    :: Color     -- ^ Coloring of the smaller part.
  , north    :: SBool     -- ^ If 'True', smaller part is to the north otherwise to the south.
  , west     :: SBool     -- ^ If 'True', smaller part is to the west, else to the east.
  , lPath    :: Path      -- ^ A path from the larger part to its capital.
  , sPath    :: Path      -- ^ A path from the smaller part to its capital.
  }

-- | A pointer to the next square on the way to the capital.
--
data Path = Path
  { distance :: SInteger  -- ^ How far to the capital?  If zero, then we are at the capital.
                          --   If negative, we are not connected to a capital (neutral territory).
  , forward  :: SBool     -- ^ Is forward (S/E) the direction to the capital?
  , vertical :: SBool     -- ^ Is moving vertically the direction to the capital (or horizontally)?
  }

-- | Is this square the capital of a country?
--
capital :: Square -> SBool
capital = (0 .==) . distance . lPath

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
    <*> mkPath   (name "LPath")
    <*> mkPath   (name "SPath")
  where
    name s = concat [ s, "[", show row, ",", show col, "]" ]

-- | Given a name prefix, create an empty symbolic 'Path'.
mkPath :: String -> Symbolic Path
mkPath prefix = Path
    <$> symbolic (prefix ++ ".Distance")
    <*> symbolic (prefix ++ ".Forward")
    <*> symbolic (prefix ++ ".Vertical")

-- * Constraining the board: adjacent squares need to fit
------------------------------------------------------------------------

-- | Do the colors of the squares fit together?
--
validColoring :: Board -> SBool
validColoring b =
  sAll (allAdjacent matchHorizontally) b
  .&&
  sAll (allAdjacent matchVertically) (transpose b)
  .&&
  allPairs notCompetingCapitals (concat b)

-- | Test all adjacent squares in a row.
allAdjacent :: (a -> a -> SBool) -> [a] -> SBool
allAdjacent rel as = allZip rel as (drop 1 as)

-- | Lift binary relation conjunctively to lists.
allZip :: (a -> a -> SBool) -> [a] -> [a] -> SBool
allZip rel xs ys = sAnd $ zipWith rel xs ys

-- | Test all distinct pairs (choose 2).
-- The relation should be symmetric.
allPairs :: (a -> a -> SBool) -> [a] -> SBool
allPairs rel = allAdjacent (allZip rel) . tails

-- | A country cannot have two capitals.
--
notCompetingCapitals :: Square -> Square -> SBool
notCompetingCapitals x y = sNot $ capital x .&& capital y .&& large x .== large y

-- | Color of the Northern edge.
northColor :: Square -> Color
northColor (Square l s n _ _ _) = ite n s l

-- | Color of the Southern edge.
southColor :: Square -> Color
southColor (Square l s n _ _ _) = ite n l s

-- | Color of the Western edge.
westColor :: Square -> Color
westColor (Square l s _ w _ _) = ite w s l

-- | Color of the Eastern edge.
eastColor :: Square -> Color
eastColor (Square l s _ w _ _) = ite w l s

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
