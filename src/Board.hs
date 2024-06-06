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
-- Irrelevant if we are neutral territory.
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

-- | Is this square split between two countries?
--
split :: Square -> SBool
split sq = large sq ./= small sq

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
validColoring b = sAnd
  [ sAll (allAdjacent matchHorizontally) b
  , sAll (allAdjacent matchVertically) (transpose b)
  , noCompetingCapitals b
  , sAll connected (concat b)
  , stayInside b
  ]
-- | Test all adjacent squares in a row.
allAdjacent :: (a -> a -> SBool) -> [a] -> SBool
allAdjacent rel as = allZip rel as (drop 1 as)

-- | Lift binary relation conjunctively to lists.
allZip :: (a -> a -> SBool) -> [a] -> [a] -> SBool
allZip rel xs ys = sAnd $ zipWith rel xs ys

-- | Test all distinct pairs (choose 2).
-- The relation should be symmetric.
allPairs :: (a -> a -> SBool) -> [a] -> SBool
allPairs rel as = sAll (allZip rel as) (tail (tails as))

-- | Invariant of a square:
-- If any part of the square belongs to a country, it needs to be connected to a capital.
connected :: Square -> SBool
connected sq = sAnd
  [ large sq .>= 0 .=> distance (lPath sq) .>= 0
  , small sq .>= 0 .=> distance (sPath sq) .> 0
  ]

-- | No country should have two capitals.
--
noCompetingCapitals :: Board -> SBool
noCompetingCapitals = allPairs notCompetingCapitals . concat

-- | A country cannot have two capitals.
--
notCompetingCapitals :: Square -> Square -> SBool
notCompetingCapitals x y = sNot $ capital x .&& capital y .&& large x .== large y

-- | Does a country have a capital?
--
hasCapital :: Color -> Board -> SBool
hasCapital i = sAny (isCapitalOf i) . concat

-- | Is the square the capital of the given country?
--
isCapitalOf :: Color -> Square -> SBool
isCapitalOf i sq = large sq .== i .&& capital sq

-- | When do two vertically adjacent squares match?
--
-- The south edge of the top square needs to have
-- the same color as the north edge of the bottom square.
matchVertically ::
     Square  -- ^ Top square.
  -> Square  -- ^ Botton square.
  -> SBool
matchVertically t b = sAnd
  [ linesConnect t b
  , southColor t .== northColor b
  , crossing sTrue (southPath t) (northPath b)
  , split t .=> noMoving sTrue  sTrue (northPath t)
  , split b .=> noMoving sFalse sTrue (southPath b)
  ]
-- | When do two horizontally adjacent squares match?
matchHorizontally ::
     Square  -- ^ Left square.
  -> Square  -- ^ Right square.
  -> SBool
matchHorizontally l r = sAnd
  [ linesConnect l r
  , eastColor l .== westColor r
  , crossing sFalse (eastPath l) (westPath r)
  , split l .=> noMoving sTrue  sFalse (westPath l)
  , split r .=> noMoving sFalse sFalse (eastPath r)
  ]

-- | Don't leave the board
stayInside :: Board -> SBool
stayInside b = sAnd
  [ sAll noUp    $ head b
  , sAll noDown  $ last b
  , sAll noLeft  $ map head b
  , sAll noRight $ map last b
  ]

noUp :: Square -> SBool
noUp sq = noMoving sFalse sTrue (lPath sq) .&& noMoving sFalse sTrue (sPath sq)

noDown :: Square -> SBool
noDown sq = noMoving sTrue sTrue (lPath sq) .&& noMoving sTrue sTrue (sPath sq)

noLeft :: Square -> SBool
noLeft sq = noMoving sFalse sFalse (lPath sq) .&& noMoving sFalse sFalse (sPath sq)

noRight :: Square -> SBool
noRight sq = noMoving sTrue sFalse (lPath sq) .&& noMoving sTrue sFalse (sPath sq)

-- | Cannot move in the given direction.
noMoving :: SBool -> SBool -> Path -> SBool
noMoving fwd vert (Path _ f v) = fwd ./= f .|| vert ./= v

-- | Crossing a border between two adjacent squares.
crossing :: SBool -> Path -> Path -> SBool
crossing vert (Path ld lf lv) (Path rd rf rv) =
  ld .>= 0 .&& rd .>= 0 .=> sAnd
    [ lf      .&& lv .== vert .=> ld .== rd + 1  -- going forward (right/down)
    , sNot rf .&& rv .== vert .=> rd .== ld + 1  -- going backward (left/up)
    , sOr
      [ lf      .&& lv .== vert .&& ld .== rd + 1  -- going forward (right/down)
      , sNot rf .&& rv .== vert .&& rd .== ld + 1  -- going backward (left/up)
      ]
    ]
-- crossing vert (Path ld lf lv) (Path rd rf rv) = sOr
--   [ ld .< 0                                    -- not a path
--   , rd .< 0                                    -- not a path
--   , lf      .&& lv .== vert .&& ld .== rd + 1  -- going forward (right/down)
--   , sNot rf .&& rv .== vert .&& rd .== ld + 1  -- going backward (left/up)
--   ]

-- | When do lines of adjacent squares connect?
-- A line crosses two adjacent of the four sectors N, W, S, E of a square.
-- Two lines connect if they share exactly one of these sectors.
-- E.g. NW connects to SW and NE, but not to SE or NW.
--
linesConnect :: Square -> Square -> SBool
linesConnect x y = (north x .== north y) .<+> (west x .== west y)

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

-- | Path of the Northern edge.
northPath :: Square -> Path
northPath sq@(Square _ _ n _ l s) = ite (split sq) (ite n s l) l

-- | Path of the Southern edge.
southPath :: Square -> Path
southPath sq@(Square _ _ n _ l s) = ite (split sq) (ite n l s) l

-- | Path of the Western edge.
westPath :: Square -> Path
westPath sq@(Square _ _ _ w l s) = ite (split sq) (ite w s l) l

-- | Path of the Eastern edge.
eastPath :: Square -> Path
eastPath sq@(Square _ _ _ w l s) = ite (split sq) (ite w l s) l

-- * Boilerplate

instance Mergeable Path where
  symbolicMerge f t p1 p2 = tupToPath $ symbolicMerge f t (pathToTup p1) (pathToTup p2)
  select ps p ind         = tupToPath $ select (map pathToTup ps) (pathToTup p) ind

pathToTup :: Path -> (SInteger, SBool, SBool)
pathToTup (Path x y z) = (x, y, z)

tupToPath :: (SInteger, SBool, SBool) -> Path
tupToPath (x, y, z) = Path x y z
