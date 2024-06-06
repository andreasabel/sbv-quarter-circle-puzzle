-- | Values of squares and puzzle definitions.

module Val where

import Data.SBV

type Puzzle = [[Val]]

type Val = (Integer, Integer)

-- Value scaling
scale = 4

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
