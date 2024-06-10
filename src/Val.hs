-- | Values of squares and puzzle definitions.

module Val where

import Data.Bifunctor (first)
import Data.Data (Data)
import Data.SBV
import Data.SBV.Internals (SBV(SBV), CV)
import Data.SBV.Internals qualified as SI

type Puzzle = [[Val]]

-- | A value is a integer linear combination of 1 and π/4.
--
type Val = (Integer, Integer)

-- | Print a 'Val' as it value in form of linear combination of 1 and π.
--
prettyVal :: Val -> String
prettyVal = \case
    (x, 0) -> show x
    (0, y) -> if y `mod` 4 == 0 then showUnless1 (y `div` 4) ++ "π" else showUnless1 y ++ "π/4"
    (x, y) -> prettyVal (x, 0) ++ " + " ++ prettyVal (0, y)
    where
      showUnless1 1 = ""
      showUnless1 n = show n

-- | Addition is pointwise, and we have only scalar multiplication.
-- 'Val' is not really a 'Num' just a kind of vector space (a module).
--
instance Num Val where
  fromInteger i       = (i, 0)
  negate (x, y)       = (negate x, negate y)
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  (n, 0) * (x, y)     = (n * x, n * y)
  abs = undefined
  signum = undefined

-- | Unit circle.
--
π :: Val
π = (0, 4)

-- | Half circle.
--
π½ :: Val
π½ = (0, 2)

-- | Quarter circle.
--
π¼ :: Val
π¼ = (0, 1)

-- | Unit square minus quarter circle.
--
π¼ᵒᵖ :: Val
π¼ᵒᵖ = (1, -1)

-- | Simple puzzle solving to a circle.
--
puzzle0 :: Puzzle
puzzle0 =
  [ [ π, π ]
  , [ π, π ]
  ]

-- | Puzzle given with model solution.
--
puzzle1 :: Puzzle
puzzle1 =
  [ [ 0, 1, 6, 0]
  , [ 6, 0, π, 2]
  , [ 1, π, 0, 6]
  , [ 0, 2, 6, 0]
  ]
