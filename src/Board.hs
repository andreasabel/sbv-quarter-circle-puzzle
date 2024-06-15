-- | The board contains the solution in a form beneficial for SMT
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
-- and each other square belonging to the country should have a finite path to this capital.
-- We represent these paths locally in a square
-- by a distance to the capital (to rule out infinite paths),
-- and a direction in which the capital lies.
-- Going in this direction brings us a step closer to the capital.
--
-- Let us assume a traveller seeking the capital looks SE,
-- then he can step to an adjacent square by being told
-- whether to move horizontally or vertically,
-- and whether to go forward or backward.

module Board where

import Control.Monad
import Data.SBV
import GHC.Generics
-- Experiment iso-deriving:
-- import Iso.Deriving  ( As(As), Isomorphic, Inject(inj), Project(prj) )

import Util

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
  , quadrant :: Quadrant  -- ^ If divided, by which quadrant of the circle?
                          --   E.g., if by the NW quadrant, then the smaller part is in the NW.
  , lPath    :: Path      -- ^ A path from the larger part to its capital.
  , sPath    :: Path      -- ^ A path from the smaller part to its capital.
  }

-- | The two main axes divide a square into four quadrants: NW SW NE SE.
--
data Quadrant = Quadrant
  { north    :: SBool     -- ^ Northern hemisphere?  (Or southern?)
  , west     :: SBool     -- ^ Western hemisphere?   (Or eastern?)
  }
  deriving (Generic, Mergeable)

-- | The first step of the path from the current square to the capital.
--
-- Irrelevant if we are neutral territory.
data Path = Path
  { distance  :: SInteger    -- ^ How far to the capital?  If zero, then we are at the capital.
                             --   If negative, we are not connected to a capital (neutral territory).
  , direction :: SDirection  -- ^ Direction (N,W,S,E) for the next step.
  }
  deriving (Generic, Mergeable)
  -- deriving Mergeable via ((SInteger, SBool, SBool) `As` Path)

-- | A pointer to the next square on the way to the capital.
--
-- The traveller to the capital looks SE.
data SDirection = Direction
  { forward  :: SBool     -- ^ Is forward (S/E) the direction to the capital?
  , vertical :: SBool     -- ^ Is moving vertically the direction to the capital (or horizontally)?
  }
  deriving (Generic, EqSymbolic, Mergeable)

-- | Quadrant north-west.
qNW :: Quadrant
qNW = Quadrant sTrue sTrue

-- | Quadrant north-east.
qNE :: Quadrant
qNE = Quadrant sTrue sFalse

-- | Quadrant south-east.
qSE :: Quadrant
qSE = Quadrant sFalse sFalse

-- | Quadrant south-west.
qSW :: Quadrant
qSW = Quadrant sFalse sTrue

-- | Direction north.
dN :: SDirection
dN = Direction sFalse sTrue

-- | Direction south.
dS :: SDirection
dS = Direction sTrue sTrue

-- | Direction west.
dW :: SDirection
dW = Direction sFalse sFalse

-- | Direction east.
dE :: SDirection
dE = Direction sTrue sFalse

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
mkRow row colShape = zipWithM (\ col _ -> mkSquare row col) [0..] colShape

-- | Create an empty square at the given coordinates.
--
mkSquare :: Int -> Int -> Symbolic Square
mkSquare row col = Square
    <$> symbolic   (name "Large")
    <*> symbolic   (name "Small")
    <*> mkQuadrant (name "Quadrant")
    <*> mkPath     (name "LPath")
    <*> mkPath     (name "SPath")
  where
    name s = concat [ s, "[", show row, ",", show col, "]" ]

-- | Given a name prefix, create an empty symbolic quadrant.
--
mkQuadrant :: String -> Symbolic Quadrant
mkQuadrant prefix = Quadrant
    <$> symbolic (prefix ++ ".North")
    <*> symbolic (prefix ++ ".West")

-- | Given a name prefix, create an empty symbolic 'Path'.
--
mkPath :: String -> Symbolic Path
mkPath prefix = Path
    <$> symbolic (prefix ++ ".Distance")
    <*> mkDirection prefix

-- | Given a name prefix, create an empty symbolic 'Direction'.
--
mkDirection :: String -> Symbolic SDirection
mkDirection prefix = Direction
    <$> symbolic (prefix ++ ".Forward")
    <*> symbolic (prefix ++ ".Vertical")

-- * Constraining the board
------------------------------------------------------------------------

-- | Is the board partitioned correctly into countries?
--
validColoring :: Board -> SBool
validColoring b = sAnd
  [ validDivision b
  , matchingColors b
  , noCompetingCapitals b
  , sAll connected (concat b)
  , stayInside b
  , sAll (allAdjacent moveHorizontally) b
  , sAll (allAdjacent moveVertically) (transpose b)
  ]

-- ** Lines need to form closed areas
------------------------------------------------------------------------

-- | A board is divided correctly into areas if no line suddenly ends.
--
validDivision :: Board -> SBool
validDivision = allSquares linesConnect

-- | When do lines at the center of 4 adjacent squares connect?
-- If the number of lines connecting to this center is not exactly one.
--
linesConnect :: (Square, Square) -> (Square, Square) -> SBool
linesConnect (nw, sw) (ne, se) = notExactlyOne
  (fallingDiag nw)
  (risingDiag sw)
  (risingDiag ne)
  (fallingDiag se)

-- | Does the square have a falling diagonal (SW or NE quadrant)?
fallingDiag :: Square -> SBool
fallingDiag (Square l s (Quadrant n w) _ _) = l ./= s .&& n ./= w

-- | Does the square have a rising diagonal (NW or SE quadrant)?
risingDiag :: Square -> SBool
risingDiag (Square l s (Quadrant n w) _ _) = l ./= s .&& n .== w

-- ** Adjacent areas need to match in color
------------------------------------------------------------------------

-- | Horizontal and vertical neighbors needs to have matching colors.
--
matchingColors :: Board -> SBool
matchingColors b = sAnd
  [ sAll (allAdjacent matchHorizontally) b
  , sAll (allAdjacent matchVertically) (transpose b)
  ]

-- | When do two horizontally adjacent squares match?
--
-- The east edge of the left square needs to have
-- the same color as the west edge of the right square.
--
matchHorizontally ::
     Square  -- ^ Left square.
  -> Square  -- ^ Right square.
  -> SBool
matchHorizontally l r = eastColor l .== westColor r

-- | When do two vertically adjacent squares match?
--
-- The south edge of the top square needs to have
-- the same color as the north edge of the bottom square.
--
matchVertically ::
     Square  -- ^ Top square.
  -> Square  -- ^ Botton square.
  -> SBool
matchVertically t b = southColor t .== northColor b

-- | Color of the Northern edge.
northColor :: Square -> Color
northColor (Square l s (Quadrant n _) _ _) = ite n s l

-- | Color of the Southern edge.
southColor :: Square -> Color
southColor (Square l s (Quadrant n _) _ _) = ite n l s

-- | Color of the Western edge.
westColor :: Square -> Color
westColor (Square l s (Quadrant _ w) _ _) = ite w s l

-- | Color of the Eastern edge.
eastColor :: Square -> Color
eastColor (Square l s (Quadrant _ w) _ _) = ite w l s

-- ** Countries need exactly one capital.
------------------------------------------------------------------------

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

-- ** Each part of a country needs to be connected to a capital.
------------------------------------------------------------------------

-- | If any part of the square belongs to a country, it needs to be connected to a capital.
--   This means that its distance should be defined.
--
connected :: Square -> SBool
connected sq = sAnd
  [ large sq .>= 0 .=> distance (lPath sq) .>= 0
  , small sq .>= 0 .=> distance (sPath sq) .> 0
  ]

-- ** A path to the capital cannot leave the board.
------------------------------------------------------------------------

-- | No path leaves the board.
--
stayInside :: Board -> SBool
stayInside b = sAnd
  [ sAll noUp    $ head b
  , sAll noDown  $ last b
  , sAll noLeft  $ map head b
  , sAll noRight $ map last b
  ]

noUp :: Square -> SBool
noUp sq = noMoving dN (lPath sq) .&& noMoving dN (sPath sq)

noDown :: Square -> SBool
noDown sq = noMoving dS (lPath sq) .&& noMoving dS (sPath sq)

noLeft :: Square -> SBool
noLeft sq = noMoving dW (lPath sq) .&& noMoving dW (sPath sq)

noRight :: Square -> SBool
noRight sq = noMoving dE (lPath sq) .&& noMoving dE (sPath sq)

-- | Cannot move in the given direction.
--
noMoving :: SDirection -> Path -> SBool
noMoving d1 (Path dist d2) = dist .<= 0 .|| d1 ./= d2

-- ** Moving correctly between squares.
------------------------------------------------------------------------

moveVertically ::
     Square  -- ^ Top square.
  -> Square  -- ^ Botton square.
  -> SBool
moveVertically t b = sAnd
  [ crossing sTrue (southPath t) (northPath b)
  , split t .=> noMoving dS (northPath t)
  , split b .=> noMoving dN (southPath b)
  ]

moveHorizontally ::
     Square  -- ^ Left square.
  -> Square  -- ^ Right square.
  -> SBool
moveHorizontally l r = sAnd
  [ crossing sFalse (eastPath l) (westPath r)
  , split l .=> noMoving dE (westPath l)
  , split r .=> noMoving dW (eastPath r)
  ]

-- | Crossing a border between two adjacent squares.
--
-- Also ensure we decrease the distance when moving a square.
--
crossing :: SBool -> Path -> Path -> SBool
crossing vert (Path ld (Direction lf lv)) (Path rd (Direction rf rv)) =
  sAnd
    [ rd .>= 0 .&& lf      .&& lv .== vert .=> ld .== rd + 1  -- going forward (right/down)
    , ld .>= 0 .&& sNot rf .&& rv .== vert .=> rd .== ld + 1  -- going backward (left/up)
    ]

-- | Path of the Northern edge.
northPath :: Square -> Path
northPath sq@(Square _ _ (Quadrant n _) l s) = ite (split sq) (ite n s l) l

-- | Path of the Southern edge.
southPath :: Square -> Path
southPath sq@(Square _ _ (Quadrant n _) l s) = ite (split sq) (ite n l s) l

-- | Path of the Western edge.
westPath :: Square -> Path
westPath sq@(Square _ _ (Quadrant _ w) l s) = ite (split sq) (ite w s l) l

-- | Path of the Eastern edge.
eastPath :: Square -> Path
eastPath sq@(Square _ _ (Quadrant _ w) l s) = ite (split sq) (ite w l s) l

-- -- * Boilerplate
-- ------------------------------------------------------------------------

-- Mergeable hand-implemented:

-- instance Mergeable Path where
--   symbolicMerge f t p1 p2 = tupToPath $ symbolicMerge f t (pathToTup p1) (pathToTup p2)
--   select ps p ind         = tupToPath $ select (map pathToTup ps) (pathToTup p) ind

-- Mergeable via iso-deriving:

-- instance (Isomorphic a b, Mergeable a) => Mergeable (As a b) where
--   symbolicMerge f t x1 x2 = As . inj @a $ symbolicMerge f t (prj @a . getAs $ x1) (prj @a . getAs $ x2)
--   select xs x ind         = As . inj @a $ select (map (prj @a . getAs) xs) (prj @a . getAs $ x) ind

-- getAs :: As a b -> b
-- getAs (As x) = x

-- instance Isomorphic (SInteger, SBool, SBool) Path where

-- instance Inject (SInteger, SBool, SBool) Path where
--   inj = tupToPath

-- instance Project (SInteger, SBool, SBool) Path where
--   prj = pathToTup

-- pathToTup :: Path -> (SInteger, SBool, SBool)
-- pathToTup (Path x y z) = (x, y, z)

-- tupToPath :: (SInteger, SBool, SBool) -> Path
-- tupToPath (x, y, z) = Path x y z

-- * Testing

-- | TODO: this manual solution might have bugs still...
--
board1 :: Board
board1 =
  [ [ Square 0 1 qNE (Path 4 dS) (Path 1 dE)
    , Square 1 0 qSE (Path 0 dO) (Path 1 dE)
    , Square 0 2 qNE (Path 0 dO) (Path 2 dE)
    , Square 2 2 qOO (Path 1 dS) pOO
    ]
  , [ Square 0 3 qSW (Path 3 dE) (Path 1 dS)
    , Square 4 0 qNW (Path 1 dS) (Path 2 dN)
    , Square 4 0 qNE (Path 2 dW) (Path 1 dN)
    , Square 2 0 qSW (Path 0 dO) (Path 2 dW)
    ]
  , [ Square 3 6 qSE (Path 0 dO) (Path 2 dS)
    , Square 4 6 qSW (Path 0 dO) (Path 1 dS)
    , Square 4 0 qSE (Path 1 dW) (Path 4 dE)
    , Square 0 0 qOO (Path 3 dN) pOO
    ]
  , [ Square 6 n qSW (Path 1 dE) pOO
    , Square 6 n qSE (Path 0 dO) pOO
    , Square 0 n qSW (Path 5 dN) pOO
    , Square 0 n qSE (Path 4 dN) pOO
    ]
  ]
  where
    qOO = qNE
    dO  = dN
    pOO = Path 0 dO
    n   = -1
