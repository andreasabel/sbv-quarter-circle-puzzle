import Data.SBV

type Val = (Integer, Integer)

instance Num Val where
  fromInteger i       = (i, 0)
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  (*)    = undefined
  abs    = undefined
  negate = undefined
  signum = undefined

main = satWith z3{ verbose = True } $ do
  x :: SBV Val <- symbolic "x"
  y :: SBV Val <- symbolic "y"
  pure $ x + y .== 1
