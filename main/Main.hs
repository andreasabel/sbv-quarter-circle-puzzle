import Control.Monad
import Data.Bifunctor
import Data.SBV
import Data.SBV.Control

import Val -- (Puzzle, puzzle0, puzzle1)
import Board
import Valuation -- (solvable, puzzle)
import Solution qualified
import Util

main = do
  -- print . unliteral $ matchHorizontally sq00 sq01
  -- print . unliteral $ matchHorizontally sq01 sq02
  -- print . unliteral $ matchHorizontally sq02 sq03

  -- print . unliteral $ matchHorizontally sq10 sq11
  -- print . unliteral $ matchHorizontally sq11 sq12
  -- print . unliteral $ matchHorizontally sq12 sq13

  -- let l = sq12
  -- let r = sq13
  -- print . unliteral $ linesConnect l r
  -- print . unliteral $ eastColor l .== westColor r
  -- print . unliteral $ crossing sFalse (eastPath l) (westPath r)
  -- print . unliteral $ split l .=> noMoving dE (westPath l)
  -- print . unliteral $ split r .=> noMoving dW (eastPath r)

  -- let ep = eastPath l
  -- let wp = westPath r
  -- print . unliteral . distance $ ep
  -- print . unliteral . distance $ wp

  -- forM_ (zip [0..] b) \ (r, row) -> do
  --   print r
  --   test $ allAdjacent matchHorizontally row

  test $ sAll (allAdjacent matchHorizontally) b
  test $ sAll (allAdjacent matchVertically) (transpose b)
  test $ noCompetingCapitals b
  test $ sAll connected (concat b)
  test $ stayInside b
  test $ validValuation b p
  -- print . unliteral $ hasCapital (literal 1) b
  -- putStrLn "validSquare (0, 1)"
  -- print . unliteral $ validSquare b sq01 1
  -- print . bimap unliteral unliteral $ boardVal (literal 1) b
  -- print . bimap unliteral unliteral $ squareVal (literal 1) sq00
  -- print . bimap unliteral unliteral $ squareVal (literal 1) sq01
  -- forDZip3M_ coordinates board1 puzzle1 \ c sq v -> do
  --   print c
  --   test $ validSquare b sq v
  where
    b = board1
    p = puzzle1
    test x = do
      satWith cfg x >>= \case
        SatResult Satisfiable{} -> return ()
        r -> error $ show r
    cfg = z3{ verbose = True }
    forDZip3M_ xs ys zs k = sequence_ $ concat $ zipWith3 (zipWith3 k) xs ys zs
    sq00 = head . head $ board1
    sq01 = head . tail . head $ board1
    sq02 = head . tail . tail . head $ board1
    sq03 = head . tail . tail . tail . head $ board1
    sq10 = head . head . tail $ board1
    sq11 = head . tail . head . tail $ board1
    sq12 = head . tail . tail . head . tail $ board1
    sq13 = head . tail . tail . tail . head . tail $ board1

puzzle2 :: Puzzle
puzzle2 =
  [ [ 2, 0 ]
  , [ 0, 2 ]
  ]

puzzle3 :: Puzzle
puzzle3 =
  [ [ 2, 0 ]
  , [ 0, 0 ]
  ]

puzzle4 :: Puzzle
puzzle4 =
  [ [ 2, 0 ]
  , [ 1, 0 ]
  ]

puzzle5 :: Puzzle
puzzle5 =
  [ [ 2, 0 ]
  , [ 1, 1 ]
  ]

puzzle6 :: Puzzle
puzzle6 =
  [ [ 1, 1 ]
  , [ 1, 1 ]
  ]

puzzle7 :: Puzzle
puzzle7 =
  [ [ 3, 0, 0 ]
  , [ 0, 0, 0 ]
  , [ 2, 0, π ]
  ]

puzzle8 :: Puzzle
puzzle8 =
  [ [ 3, 0, 0, 4 ]
  , [ 0, 0, 0, 0 ]
  , [ 2, 0, π, 0 ]
  , [ 0, 0, 0, 0 ]
  ]

main_ = print =<< Solution.solve puzzle1
