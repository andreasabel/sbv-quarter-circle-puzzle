import Data.SBV

type Val = (Integer, Integer)

instance Num Val where
  fromInteger i       = (i, 0)
  negate (x, y)       = (negate x, negate y)
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  (*)    = undefined
  abs    = undefined
  signum = undefined

type SVal = SBV Val

valVec :: Int -> Symbolic [SVal]
valVec n = mapM (\ k -> symbolic ("X" ++ show k)) [0..n-1]

main = satWith z3{ verbose = True } $ do
  v <- valVec 3
  pure $ sum v .== 1
