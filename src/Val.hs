-- | Values of squares and puzzle definitions.

module Val where

import Data.Bifunctor (first)
import Data.Data (Data)
import Data.SBV
import Data.SBV.Internals (SBV(SBV), CV)
import Data.SBV.Internals qualified as SI

type Puzzle = [[Val]]

type Val = (Integer, Integer)

-- Value scaling
scale = 4

prettyVal :: Val -> String
prettyVal = \case
    (x, 0) -> if x `mod` scale == 0 then show (x `div` scale) else show x ++ "/" ++ show scale
    (0, y) -> if y `mod` scale == 0 then showUnless1 (y `div` scale) ++ "π" else showUnless1 y ++ "π/" ++ show scale
    (x, y) -> prettyVal (x, 0) ++ " + " ++ prettyVal (0, y)
    where
      showUnless1 1 = ""
      showUnless1 n = show n

instance Num Val where
  fromInteger i       = (scale * i, 0)
  negate (x, y)       = (negate x, negate y)
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  (n, 0) * (x, y)     = (n * x `div` scale, n * y `div` scale)
  abs = undefined
  signum = undefined

-- | Unit circle.
π :: Val
π = (0, scale)

-- | Quarter circle.
π¼ :: Val
π¼ = (0, scale `div` 4)

-- | Unit square minus quarter circle.
π¼ᵒᵖ :: Val
π¼ᵒᵖ = (scale, -scale `div` 4)

puzzle0 :: Puzzle
puzzle0 =
  [ [ π, π ]
  , [ π, π ]
  ]

puzzle1 :: Puzzle
puzzle1 =
  [ [ 0, 1, 6, 0]
  , [ 6, 0, π, 2]
  , [ 1, π, 0, 6]
  , [ 0, 2, 6, 0]
  ]
