module Solution where

import Control.Monad.IO.Class
import Data.SBV
import Data.SBV.Control

import Val
import Board
import Valuation

type Solution = [[Line]]

-- | Possible square fillings.
data Line
  = OO    -- ^ Empty.
  | NW    -- ^ Quarter circle from north to west. ◜
  | SW    -- ^ Quarter circle from south to west. ◟
  | NE    -- ^ Quarter circle from north to east. ◝
  | SE    -- ^ Quarter circle from south to east. ◞
  deriving (Eq, Ord, Show)

-- TODO:
solve :: Puzzle -> IO Solution
solve p = runSMT do
  b <- mkBoard p
  constrain $ validSolution p b
  query $ checkSat >>= \case
    Sat -> do
      sol <- getSolution b
      liftIO . putStr . prettySolution $ sol
      -- print coloring
      liftIO . print =<< mapM (mapM (getValue . large)) b
      -- print distances
      liftIO . print =<< mapM (mapM (getValue . distance . lPath)) b
      -- print directions
      liftIO . print =<< mapM (mapM getDirection) b
      -- print competing capitals?
      liftIO . print =<< getValue (noCompetingCapitals b)
      return sol
    r -> error $ "Solver said: " ++ show r

getSolution :: Board -> Query Solution
getSolution = mapM (mapM getSquare)

getSquare :: Square -> Query Line
getSquare (Square l s q _ _) = do
  cl <- getValue l
  cs <- getValue s
  if cl == cs then return OO else getQuadrant q

getQuadrant :: Quadrant -> Query Line
getQuadrant (Quadrant n w) = do
  bn <- getValue n
  bw <- getValue w
  return case (bn, bw) of
    (True , True ) -> NW
    (True , False) -> NE
    (False, True ) -> SW
    (False, False) -> SE

getDirection :: Square -> Query Direction
getDirection (Square _ _ _ (Path _ (Direction f v)) _) = do
  bf <- getValue f
  bv <- getValue v
  return
    case (bf, bv) of
      (True , True ) -> S
      (True , False) -> E
      (False, True ) -> N
      (False, False) -> W

data Direction = N | W | S | E
  deriving Show

-- * Pretty printing
------------------------------------------------------------------------

prettyLine :: Line -> Char
prettyLine = \case
  OO -> ' '
  NW -> '◜'
  SW -> '◟'
  NE -> '◝'
  SE -> '◞'

prettySolution' :: Solution -> [String]
prettySolution' = map (map prettyLine)

prettySolution :: Solution -> String
prettySolution = unlines . prettySolution'
