import Data.SBV
import Valuation

main = sat (solvable [[1]])
