module Board where

type Position = (Int, Int)

data Player = P1 | P2
  deriving (Eq, Ord)

data Cell = Empty | PlayerCell Player
  deriving Eq

data GameOver = Winner Player | DrawnGame
  deriving Show

class Show a => Board a where
  getFreeCells :: a -> [Position]
  legalMove :: Position -> a -> Bool
  makeMove :: Player -> Position -> a -> a
  getWinner :: a -> Maybe GameOver
  fromString :: String -> a
  emptyBoard :: a

renderCell :: Cell -> Char
renderCell (PlayerCell P1) = 'X'
renderCell (PlayerCell P2) = 'O'
renderCell Empty = '.'

readCell :: Char -> Cell
readCell 'X' = PlayerCell P1
readCell 'O' = PlayerCell P2
readCell '.' = Empty

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

instance Show Player where
  show P1 = "X"
  show P2 = "O"

instance Show Cell where
  show = (:[]) . renderCell
