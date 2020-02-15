module Lib
    ( playGame
    ) where

import Data.Char (isDigit, digitToInt)

import Board
import ListBoard

playGame :: IO ()
playGame = do
  gameOver <- playUntilDone P1 (emptyBoard :: ListBoard)
  case gameOver of
    Winner winner -> putStrLn $ "Game over! Winner: " ++ show winner
    DrawnGame     -> putStrLn "It's a draw!"

playUntilDone :: Board b => Player -> b -> IO GameOver
playUntilDone player board = do
  (board', maybeGameOver) <- oneMove player board
  case maybeGameOver of
    Just gameOver -> print board' >> return gameOver
    Nothing -> playUntilDone (nextPlayer player) board'

oneMove :: Board b => Player -> b -> IO (b, Maybe GameOver)
oneMove player board = do
  print board
  putStrLn $ "Enter move for " ++ show player ++ ", format 'x y':"
  input <- getLine
  case parsePos input of
    Nothing -> putStrLn "Illegal input!" >> oneMove player board
    Just pos -> if legalMove pos board
                
                  then let board' = makeMove player pos board
                       in  return (board', getWinner board')
                       
                  else putStrLn "Illegal move!" >> oneMove player board

parsePos :: String -> Maybe Position
parsePos [x,' ',y] = if isDigit x && isDigit y
                       then Just (digitToInt x, digitToInt y)
                       else Nothing
parsePos _ = Nothing
