module Bot where

import Data.List (sortBy)

import Board

selectMove :: Board b => Player -> b -> (Position, GameOver)
selectMove player board = head $ scores player board

scores :: Board b => Player -> b -> [(Position, GameOver)]
scores player board =
  let legalMoves  = getFreeCells board
      scoreMove p = score player (makeMove player p board)
      results     = map (\pos -> (pos, scoreMove pos)) legalMoves
  in  sortBy (winSorter player) results

score :: Board b => Player -> b -> GameOver
score lastPlayer board =
  let nextPlayer' = nextPlayer lastPlayer
      bestMove    = snd $ head $ scores nextPlayer' board
  in  case getWinner board of
        Just gameOver -> gameOver
        Nothing -> bestMove

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
