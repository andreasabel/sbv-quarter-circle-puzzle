module Solution where

import Data.SBV
import Data.SBV.Control

import Val
import Board
import Valuation

type Solution = [[Line]]

-- | Possible square fillings.
data Line
  = OO    -- ^ Empty.
  | NW    -- ^ Quarter circle from north to west.
  | SW    -- ^ Quarter circle from south to west.
  | NE    -- ^ Quarter circle from north to east.
  | SE    -- ^ Quarter circle from south to east.
  deriving (Eq, Ord, Show)

-- TODO:
solve :: Puzzle -> IO Solution
solve p = runSMT do
  b <- mkBoard p
  constrain $ validSolution p b
  query $ checkSat >>= \case
    Sat -> getSolution b
    r -> error $ "Solver said: " ++ show r

getSolution :: Board -> Query Solution
getSolution = mapM (mapM getSquare)

getSquare :: Square -> Query Line
getSquare (Square l s n w _) = do
  cl <- getValue l
  cs <- getValue s
  bn <- getValue n
  bw <- getValue w
  return if cl == cs then OO else
    case (bn, bw) of
      (True , True ) -> NW
      (True , False) -> NE
      (False, True ) -> SW
      (False, False) -> SE
