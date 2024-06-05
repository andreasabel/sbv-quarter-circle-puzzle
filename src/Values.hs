-- | Values of squares and puzzle definitions.

module Values where

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
  Val n 0 * Val x y     = Val (n * x `div` scale) (n * y `div` scale)
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
