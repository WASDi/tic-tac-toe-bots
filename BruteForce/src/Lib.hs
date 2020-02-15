module Lib
    ( playGame
    ) where

import Data.Char (isDigit, digitToInt)

import Board
import ListBoard

playGame :: IO ()
playGame = do
  winner <- playUntilDone P1 (emptyBoard :: ListBoard)
  putStrLn $ "Game over! Winner: " ++ show winner

playUntilDone :: Board b => Player -> b -> IO Player
playUntilDone player board = do
  (board', maybeWinner) <- oneMove player board
  case maybeWinner of
    Just winner -> print board' >> return winner
    Nothing -> playUntilDone (nextPlayer player) board'

oneMove :: Board b => Player -> b -> IO (b, Maybe Player)
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
