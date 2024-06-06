import Data.SBV

import Val (puzzle0, puzzle1)
import Valuation (solvable, puzzle)
import Solution qualified

-- main = satWith z3{ verbose = True } (solvable puzzle0)

main = print =<< Solution.solve puzzle1
