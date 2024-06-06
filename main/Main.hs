import Data.SBV

import Val -- (Puzzle, puzzle0, puzzle1)
import Valuation (solvable, puzzle)
import Solution qualified

-- main = satWith z3{ verbose = True } (solvable puzzle0)

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

main = print =<< Solution.solve puzzle8
