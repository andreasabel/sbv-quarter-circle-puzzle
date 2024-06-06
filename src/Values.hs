-- | Values of squares and puzzle definitions.

module Values where

import Data.Bifunctor (first)
import Data.Data (Data)
import Data.SBV
import Data.SBV.Internals (SBV(SBV), CV)
import Data.SBV.Internals qualified as SI

type Puzzle = [[Val]]

data Val = Val { quaters :: Integer, piQuaters :: Integer }
  deriving (Eq, Ord, Data)

-- Value scaling
scale = 8

-- deriving instance Show Val
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

-- | Unit circle.
π :: Val
π = Val 0 scale

-- | Quarter circle.
π¼ :: Val
π¼ = Val 0 (scale `div` 4)

-- | Unit square minus quarter circle.
π¼ᵒᵖ :: Val
π¼ᵒᵖ = Val scale (-scale `div` 4)

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

-- Boilerplate SymVal instance for Val record

mapReadS :: (a -> b) -> ReadS a -> ReadS b
mapReadS f p s = map (first f) $ p s

instance Read Val where
  readsPrec = mapReadS (uncurry Val) . readsPrec

instance HasKind Val where
  kindOf ~(Val x y) = kindOf (x, y)

-- | SBV is just a phantom type, so we can coerce.
fromSBVPair :: SBV (Integer, Integer) -> SBV Val
fromSBVPair (SBV x) = SBV x

instance SymVal Val where
  -- mkSymVal :: MonadSymbolic m => VarContext -> Maybe String -> m (SBV a)
  mkSymVal cxt x = fromSBVPair <$> SI.mkSymVal cxt x

  literal :: Val -> SBV Val
  literal (Val x y) = fromSBVPair (literal (x, y))

  fromCV :: CV -> Val
  fromCV = uncurry Val . fromCV
