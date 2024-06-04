{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | Puzzle 2 of CS retreat 2024

-- import Prelude hiding (pi)

-- import Control.Monad ((<=<))
import Data.List.Extra ((!?))

import Data.Map (Map)
import Data.Map qualified as Map

main :: IO ()
main = putStrLn "Hello puzzle two!"

-- * Puzzle
------------------------------------------------------------------------

type Puzzle = [Val]

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
  fromInteger i         = Val (scale * i) 0
  negate (Val x y)      = Val (negate x) (negate y)
  Val x1 y1 + Val x2 y2 = Val (x1 + x2) (y1 + y2)
  Val n 0 * Val x y     = Val  (n * x `div` scale) (n * y `div` scale)
  abs = undefined
  signum = undefined

π :: Val
π = Val 0 scale

puzzle1 :: Puzzle
puzzle1 = concat
  [ [ 0, 1, 6, 0]
  , [ 6, 0, π, 2]
  , [ 1, π, 0, 6]
  , [ 0, 2, 6, 0]
  ]

-- * Solution
------------------------------------------------------------------------

type Solution = [Line]

-- | Possible square fillings.
data Line
  = OO    -- ^ Empty.
  | NW    -- ^ Quarter circle from north to west.
  | SW    -- ^ Quarter circle from south to west.
  | NE    -- ^ Quarter circle from north to east.
  | SE    -- ^ Quarter circle from south to east.
  deriving (Eq, Ord, Show)

solution1 :: Solution
solution1 = concat
  [ [ NE, SE, NE, OO ]
  , [ SW, NW, NE, SW ]
  , [ SE, SW, SE, OO ]
  , [ SW, SE, SW, SE ]
  ]

-- * Coloring
------------------------------------------------------------------------

-- | Cluster IDs.
type ID = Integer

data Coloring = C
  { _main :: ID  -- ^ Color of the larger area of this (possibly divided square).
  , _side :: ID  -- ^ Color of the smaller area.  (Same as 'main' if not divided).
  }
  deriving (Show)

type ColorMap = [Coloring]

coloring1 :: ColorMap
coloring1 = concat
  [ [ C 0 1, C 1 0, C 0 2, C 2 2 ]
  , [ C 0 3, C 4 0, C 4 0, C 2 0 ]
  , [ C 3 6, C 4 6, C 4 0, C 0 0 ]
  , [ C 6 5, C 6 7, C 0 7, C 0 8 ]
  ]

invariantColoring :: Coloring -> Line -> Bool
invariantColoring (C ma si) = \case
  OO -> ma == si
  _  -> ma /= si

invariantColorMap :: ColorMap -> Solution -> Bool
invariantColorMap col sol = and $ zipWith invariantColoring col sol

icm1 = invariantColorMap coloring1 solution1

-- ** Partitioning (old-style coloring)
------------------------------------------------------------------------

type Partition = [Partitioning]
type Partitioning = P ID

-- | The four edges of a square.
data P a = P { north, west, south, east :: a }
  deriving (Show, Eq, Functor, Foldable, Traversable)

zipWithP :: (a -> b -> c) -> P a -> P b -> P c
zipWithP f (P n w s e) (P n' w' s' e') = P (f n n') (f w w') (f s s') (f e e')

partition1 :: Partition
partition1 = concat
  [ [ P 1 0 0 1, P 1 1 0 0, P 2 0 0 2, P 2 2 2 2 ]
  , [ P 0 3 3 0, P 0 0 4 4, P 0 4 4 0, P 2 0 0 2 ]
  , [ P 3 3 6 6, P 4 6 6 4, P 4 4 0 0, P 0 0 0 0 ]
  , [ P 6 5 5 6, P 6 6 7 7, P 0 7 7 0, P 0 0 8 8 ]
  ]

colorPartitioning  :: Coloring -> Line -> Partitioning
colorPartitioning (C m s) = \case
  NW -> P s s m m
  SW -> P m s s m
  SE -> P m m s s
  NE -> P s m m s
  OO -> P m m m m

colorPartition :: ColorMap -> Solution -> Partition
colorPartition = zipWith colorPartitioning

colorPartition1 = colorPartition coloring1 solution1
cp1 = colorPartition1 == partition1

-- * Coordinates
------------------------------------------------------------------------

type Height = Int
type Width  = Int

data Dim = Dim { height :: Height, width :: Width  }
  deriving (Show)

dim1 :: Dim
dim1 = Dim 4 4

type Size = Int

size :: Dim -> Size
size (Dim h w) = h * w

type Coord = Int

filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe p a = if p a then Just a else Nothing

goNorth :: Dim -> Coord -> Maybe Coord
goNorth (Dim _ w)     x = filterMaybe (>= 0) $ x - w

goWest :: Dim -> Coord -> Maybe Coord
goWest (Dim _ w)      x = filterMaybe (const $ x `mod` w > 0) $ x - 1   -- prevent wrap

goSouth :: Dim -> Coord -> Maybe Coord
goSouth dim@(Dim _ w) x = filterMaybe (< size dim) $ x + w

goEast :: Dim -> Coord -> Maybe Coord
goEast (Dim _ w)      x = filterMaybe ((0 /=) . (`mod` w)) $ x + 1      -- prevent wrap

coordinates :: Dim -> [Coord]
coordinates dim = [0 .. size dim - 1]

-- * Checking coloring/partitioning
------------------------------------------------------------------------

-- | Check that the neighbors have the correct color.
validColoring :: Dim -> Partition -> Coord -> Coloring -> Line -> Bool
validColoring dim part coord c@(C ma si) line =
  and $ zipWithP (\ a -> maybe True (a ==)) p mp
  where
    p  = colorPartitioning c line
    mp = P n w s e
    n = fmap south . (part !?) =<< goNorth dim coord
    w = fmap east  . (part !?) =<< goWest  dim coord
    s = fmap north . (part !?) =<< goSouth dim coord
    e = fmap west  . (part !?) =<< goEast  dim coord

validColorMap :: Dim -> ColorMap -> Solution -> Bool
validColorMap dim col sol =
  and $ zipWith3 (validColoring dim part) (coordinates dim) col sol
  where
    part = colorPartition col sol

vcm1 = validColorMap dim1 coloring1 solution1

validSquare :: Dim -> Partition -> Coord -> Partitioning -> Line -> Bool
validSquare dim part coord (P n w s e) = \case
  -- quarter square N to W: S and E have same ID
  NW -> and [ n == w, s == e
            , maybe True ((n ==) . south) $ (part !?) =<< goNorth dim coord
            , maybe True ((w ==) . east ) $ (part !?) =<< goWest  dim coord
            ]
  SW -> and [ s == w, n == e
            , maybe True ((s ==) . north) $ (part !?) =<< goSouth dim coord
            , maybe True ((w ==) . east ) $ (part !?) =<< goWest  dim coord
            ]
  NE -> and [ n == e, s == w
            , maybe True ((n ==) . south) $ (part !?) =<< goNorth dim coord
            , maybe True ((e ==) . west ) $ (part !?) =<< goEast  dim coord
            ]
  SE -> and [ s == e, n == w
            , maybe True ((s ==) . north) $ (part !?) =<< goSouth dim coord
            , maybe True ((e ==) . west ) $ (part !?) =<< goEast  dim coord
            ]
  OO -> and [ n == w, w == s, s == e
            , maybe True ((n ==) . south) $ (part !?) =<< goNorth dim coord
            , maybe True ((w ==) . east ) $ (part !?) =<< goWest  dim coord
            , maybe True ((s ==) . north) $ (part !?) =<< goSouth dim coord
            , maybe True ((e ==) . west ) $ (part !?) =<< goEast  dim coord
            ]

validPartition :: Dim -> Partition -> Solution -> Bool
validPartition dim part sol = and $
  zipWith3 (validSquare dim part) (coordinates dim) part sol

validPartition1 = validPartition dim1 partition1 solution1

vp n = validPartition dim1 (take n partition1) (take n solution1)

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
valuePartition part sol = Map.unionsWith (+) $ zipWith valueSquare part sol

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
validValues m puzzle part sol = and $ zipWith3 (validValue m) puzzle part sol

vv1 = validValues valuation1 puzzle1 partition1 solution1

validSolution :: Dim -> Puzzle -> Solution -> Partition -> Bool
validSolution dim puzzle sol part = and
  [ validPartition dim part sol
  , validValues m puzzle part sol
  ]
  where
    m = valuePartition part sol

vs1 = validSolution dim1 puzzle1 solution1 partition1
