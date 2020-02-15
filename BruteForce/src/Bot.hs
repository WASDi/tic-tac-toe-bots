module Bot where

import Data.List (sortBy)

import Board

selectMove :: Board b => Player -> b -> Position
selectMove player board =
  let legalMoves = getFreeCells board
      moveScores = map (\p -> (p, score player $ makeMove player p board)) legalMoves
      bestMoves  = sortBy (winSorter player) moveScores
  in  fst $ head bestMoves

data Score = Win | Draw | Lose
  deriving (Show, Ord, Eq)

score :: Board b => Player -> b -> GameOver
score player board =
  let legalMoves  = getFreeCells board
      enemy       = nextPlayer player
      scoreMove p = score enemy $ makeMove player p board
      results     = map (\p -> (p, scoreMove p)) legalMoves
      bestMoves   = sortBy (winSorter player) results
  in  case getWinner board of
        Just gameOver -> gameOver
        Nothing -> snd $ head bestMoves

winSorter :: Player -> (Position, GameOver) -> (Position, GameOver) -> Ordering
winSorter p (_,a) (_,b) = winSorter' a b
  where
    winSorter' :: GameOver -> GameOver -> Ordering
    winSorter' (Winner p1) (Winner p2) | p1 == p2  = EQ
                                       | p1 == p   = LT
                                       | otherwise = GT
    winSorter' (Winner p') DrawnGame = if p == p' then LT else GT
    winSorter' DrawnGame (Winner p') = if p == p' then GT else LT
    winSorter' DrawnGame DrawnGame   = EQ
