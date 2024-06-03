-- | Puzzle 2 of CS retreat 2024

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- import Prelude hiding (pi)

import Data.Map (Map)
import Data.Map qualified as Map

main :: IO ()
main = putStrLn "Hello puzzle two!"

-- * Puzzle
------------------------------------------------------------------------

type Puzzle = [[Val]]

data Val = Val { quaters :: Integer, piQuaters :: Integer }
  deriving (Eq, Ord)

-- Value scaling
scale = 8

instance Show Val where
  show = \case
    Val x 0 -> if x `mod` scale == 0 then show (x `div` scale) else show x ++ "/" ++ show scale
    Val 0 y -> if y `mod` scale == 0 then showUnless1 (y `div` scale) ++ "π" else showUnless1 y ++ "π/" ++ show scale
    Val x y -> show (Val x 0) ++ " + " ++ show (Val 0 y)
    where
      showUnless1 1 = ""
      showUnless1 n = show n

instance Num Val where
  fromInteger i = Val (scale * i) 0
  negate (Val x y) = Val (negate x) (negate y)
  Val x1 y1 + Val x2 y2 = Val (x1 + x2) (y1 + y2)
  Val n 0 * Val x y = Val  (n * x `div` scale) (n * y `div` scale)
  abs = undefined
  signum = undefined

π :: Val
π = Val 0 scale

puzzle1 :: Puzzle
puzzle1 =
  [ [ 0, 1, 6, 0]
  , [ 6, 0, π, 2]
  , [ 1, π, 0, 6]
  , [ 0, 2, 6, 0]
  ]

-- * Solution
------------------------------------------------------------------------

type Solution = [[Line]]

-- | Possible square fillings.
data Line
  = OO    -- ^ Empty.
  | NW    -- ^ Quarter circle from north to west.
  | SW    -- ^ Quarter circle from south to west.
  | NE    -- ^ Quarter circle from north to east.
  | SE    -- ^ Quarter circle from south to east.
  deriving (Eq, Ord, Show)

solution1 :: Solution
solution1 =
  [ [ NE, SE, NE, OO ]
  , [ SW, NW, NE, SW ]
  , [ SE, SW, SE, OO ]
  , [ SW, SE, SW, SE ]
  ]

-- | Cluster IDs.
type ID = Integer

type Partition = [[Partitioning]]

-- | Divide each square via the diagonals into 4 areas, each of which
-- gets an ID of its cluser.
data Partitioning = P { north, west, south, east :: ID }
  deriving (Show)

partition1 :: Partition
partition1 =
  [ [ P 1 0 0 1, P 1 1 0 0, P 2 0 0 2, P 2 2 2 2 ]
  , [ P 0 3 3 0, P 0 0 4 4, P 0 4 4 0, P 2 0 0 2 ]
  , [ P 3 3 6 6, P 4 6 6 4, P 4 4 0 0, P 0 0 0 0 ]
  , [ P 6 5 5 6, P 6 6 7 7, P 0 7 7 0, P 0 0 8 8 ]
  ]

data Dim = Dim { height, width :: Int }
  deriving (Show)

dim1 :: Dim
dim1 = Dim 4 4

data Coord = Coord { vert, horiz :: Int }
  deriving Eq

-- | Coordinate inside board.
validCoord :: Dim -> Coord -> Bool
validCoord (Dim h w) (Coord ve ho) = and [ ve >= 0, ve < h, ho >= 0, ho < w ]

goNorth :: Coord -> Maybe Coord
goNorth (Coord ve ho) = if ve > 0 then Just $ Coord (ve - 1) ho else Nothing

goWest :: Coord -> Maybe Coord
goWest (Coord ve ho) = if ho > 0 then Just $ Coord ve (ho - 1) else Nothing

goSouth :: Dim -> Coord -> Maybe Coord
goSouth (Dim h _) (Coord ve ho) = if ve < h - 1 then Just $ Coord (ve + 1) ho else Nothing

goEast :: Dim -> Coord -> Maybe Coord
goEast (Dim _ w) (Coord ve ho) = if ho < w - 1 then Just $ Coord ve (ho + 1) else Nothing

-- validPartitioning :: Dim -> Solution -> Partition -> Bool
-- validPartitioning (Dim height width)

-- sameID :: Dim -> Partition -> ID -> Maybe Coord -> Bool
-- sameID dim part i = \case
--   Nothing -> True
--   Just coord -> i == part ! coord

(!) :: [[a]] -> Coord -> a
board ! Coord ve ho = board !! ve !! ho

dzipWith2 :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
dzipWith2 f = zipWith $ zipWith f

dzipWith3 :: (a -> b -> c -> d) -> [[a]] -> [[b]] -> [[c]] -> [[d]]
dzipWith3 f = zipWith3 $ zipWith3 f

coordinates :: Dim -> [[Coord]]
coordinates (Dim height width) = map (\ ve -> map (Coord ve) [0..width-1]) [0..height-1]

validSquare :: Dim -> Partition -> Coord -> Partitioning -> Line -> Bool
validSquare dim part coord (P n w s e) = \case
  -- quarter square N to W: S and E have same ID
  NW -> and [ n == w, s == e
            , maybe True ((n ==) . south . (part !)) $ goNorth     coord
            , maybe True ((w ==) . east  . (part !)) $ goWest      coord
            ]
  SW -> and [ s == w, n == e
            , maybe True ((s ==) . north . (part !)) $ goSouth dim coord
            , maybe True ((w ==) . east  . (part !)) $ goWest      coord
            ]
  NE -> and [ n == e, s == w
            , maybe True ((n ==) . south . (part !)) $ goNorth     coord
            , maybe True ((e ==) . west  . (part !)) $ goEast  dim coord
            ]
  SE -> and [ s == e, n == w
            , maybe True ((s ==) . north . (part !)) $ goSouth dim coord
            , maybe True ((e ==) . west  . (part !)) $ goEast  dim coord
            ]
  OO -> and [ n == w, w == s, s == e
            , maybe True ((n ==) . south . (part !)) $ goNorth     coord
            , maybe True ((w ==) . east  . (part !)) $ goWest      coord
            , maybe True ((s ==) . north . (part !)) $ goSouth dim coord
            , maybe True ((e ==) . west  . (part !)) $ goEast  dim coord
            ]

validPartition :: Dim -> Partition -> Solution -> Bool
validPartition dim part sol = all and $
  dzipWith3 (validSquare dim part) (coordinates dim) part sol

validPartition1 = validPartition dim1 partition1 solution1

-- | Valuation of a partition (accumulated values).
type Valuation = Map ID Val

-- | The large (inner) part of a half quarter circle.
large :: Val
large = Val 0 1

-- | The small (outer) part of a half quarter circle.
small :: Val
small = Val 4 (0 - 1)

-- -- | A quarter square
-- quarter :: Val
-- quarter = Val 1 0

valueSquare :: Partitioning -> Line -> Valuation
valueSquare (P n w s e) = \case
  NW -> Map.fromListWith (+) [(n, small), (w, small), (s, large), (e, large)]
  SW -> Map.fromListWith (+) [(n, large), (w, small), (s, small), (e, large)]
  SE -> Map.fromListWith (+) [(n, large), (w, large), (s, small), (e, small)]
  NE -> Map.fromListWith (+) [(n, small), (w, large), (s, large), (e, small)]
  OO -> Map.singleton n 1

valuePartition :: Partition -> Solution -> Valuation
valuePartition part sol = Map.unionsWith (+) $ concat $ dzipWith2 valueSquare part sol

valuation1 = valuePartition partition1 solution1

idOf :: Partitioning -> Line -> ID
idOf (P n w s e) = \case
  NW -> s
  SW -> n
  SE -> n
  NE -> s
  OO -> n

-- valueAt :: Valuation -> Partitioning -> Line -> Val
-- valueAt

validValue :: Valuation -> Val -> Partitioning -> Line -> Bool
validValue m 0 p l = True
validValue m v p l = v == m Map.! idOf p l

validValues :: Valuation -> Puzzle -> Partition -> Solution -> Bool
validValues m puzzle part sol = all and $ dzipWith3 (validValue m) puzzle part sol

vv1 = validValues valuation1 puzzle1 partition1 solution1

validSolution :: Dim -> Puzzle -> Solution -> Partition -> Bool
validSolution dim puzzle sol part = and
  [ validPartition dim part sol
  , validValues m puzzle part sol
  ]
  where
    m = valuePartition part sol

vs1 = validSolution dim1 puzzle1 solution1 partition1
