module Board where

import Control.Monad
import Data.List (transpose)
import Data.SBV

-- | Solution
type Board = [[Square]]

-- | Coloring maps.
--
type Color = SInteger

-- | A square can have one or two colors.
--
-- In the latter case, it is divided by a quarter circle
-- which can have 4 different orientations, defined by two bits.
--
data Square = Square
  { large :: Color  -- ^ Coloring of the larger part.
  , small :: Color  -- ^ Coloring of the smaller part.
  , north :: SBool  -- ^ If 'True', smaller part is to the north otherwise to the south.
  , west  :: SBool  -- ^ If 'True', smaller part is to the west, else to the east.
  }

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
  where
    name s = concat [ s, "[", show row, ",", show col, "]" ]

-- | Do the colors of the squares fit together?
--
validColoring :: Board -> SBool
validColoring b =
  sAll (allAdjacent matchHorizontally) b
  .&&
  sAll (allAdjacent matchVertically) (transpose b)

allAdjacent :: (a -> a -> SBool) -> [a] -> SBool
allAdjacent f as = sAnd $ zipWith f as (drop 1 as)

-- | Color of the Northern edge.
northColor :: Square -> Color
northColor (Square l s n _) = ite n s l

-- | Color of the Southern edge.
southColor :: Square -> Color
southColor (Square l s n _) = ite n l s

-- | Color of the Western edge.
westColor :: Square -> Color
westColor (Square l s _ w) = ite w s l

-- | Color of the Eastern edge.
eastColor :: Square -> Color
eastColor (Square l s _ w) = ite w l s

-- | When do two vertically adjacent squares match?
matchVertically ::
     Square  -- ^ Top square.
  -> Square  -- ^ Botton square.
  -> SBool
matchVertically t b = southColor t .== northColor b

-- | When do two horizontally adjacent squares match?
matchHorizontally ::
     Square  -- ^ Left square.
  -> Square  -- ^ Right square.
  -> SBool
matchHorizontally l r = eastColor l .== westColor r
