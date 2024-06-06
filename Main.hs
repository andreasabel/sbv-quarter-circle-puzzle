import Data.SBV
import Control.Monad (zipWithM)
import Data.List (transpose)

type Puzzle = [[Val]]

type Val = (Integer, Integer)

instance Num Val where
  fromInteger i       = (4 * i, 0)
  negate (x, y)       = (negate x, negate y)
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  (n, 0) * (x, y)     = (n * x `div` 4, n * y `div` 4)
  abs = undefined
  signum = undefined

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
    (ite (s .== i) 1 (literal (0, 1)))
    (ite (s .== i) (literal (4, -1)) 0)

main = sat (solvable [[1]])
