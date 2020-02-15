module PlayGame where

import Data.Char (isDigit, digitToInt)

import Bot
import Board
import ListBoard

playAgainstBot :: Bool
playAgainstBot = True

playGame :: IO ()
playGame = do
  gameOver <- playUntilDone P1 (emptyBoard :: ListBoard)
  case gameOver of
    Winner winner -> putStrLn $ "Game over! Winner: " ++ show winner
    DrawnGame     -> putStrLn "It's a draw!"

playUntilDone :: Board b => Player -> b -> IO GameOver
playUntilDone player board = do
  (board', maybeGameOver) <- if playAgainstBot && player == P2
                               then botMove player board
                               else oneMove player board
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

-- FIXME: player (1,1), bot (0,0), player (1,2), bot (0,1), player (1,0), player win!
botMove :: Board b => Player -> b -> IO (b, Maybe GameOver)
botMove player board = do
  let pos = selectMove player board
  let board' = makeMove player pos board
  putStrLn $ "Bot places " ++ show player ++ " on... " ++ show pos
  return (board', getWinner board')

parsePos :: String -> Maybe Position
parsePos [x,' ',y] = if isDigit x && isDigit y
                       then Just (digitToInt x, digitToInt y)
                       else Nothing
parsePos _ = Nothing
