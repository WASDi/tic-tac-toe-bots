module ListBoard where

import Data.Maybe (mapMaybe)
import Board

newtype ListBoard = ListBoard [[Cell]]

instance Board ListBoard where
  getFreeCells = getFreeCells'
  legalMove = legalMove'
  makeMove = makeMove'
  getWinner = getWinner'
  fromString = fromString'
  emptyBoard = emptyBoard'

getFreeCells' :: ListBoard -> [Position]
getFreeCells' =  mapMaybe keepEmpty . concat . addPos
  where
    keepEmpty :: (Cell, Position) -> Maybe Position
    keepEmpty (Empty, p) = Just p
    keepEmpty _          = Nothing
    
    addPos :: ListBoard -> [[(Cell, Position)]]
    addPos (ListBoard rows) = map (\(row, y) -> zip row (map (\x -> (y,x)) [0..])) (zip rows [0..])

legalMove' :: Position -> ListBoard -> Bool
legalMove' (x,y) (ListBoard rows) = and [ x >= 0, x <= 2
                                        , y >= 0, y <= 2
                                        , rows !! x !! y == Empty]

makeMove' :: Player -> Position -> ListBoard -> ListBoard
makeMove' p (y,x) (ListBoard [r1,r2,r3]) = 
  let r1' = if y == 0 then setCell r1 else r1
      r2' = if y == 1 then setCell r2 else r2
      r3' = if y == 2 then setCell r3 else r3
  in  ListBoard [r1',r2',r3']
    where
      setCell [c1,c2,c3] = [ if x == 0 then PlayerCell p else c1
                           , if x == 1 then PlayerCell p else c2
                           , if x == 2 then PlayerCell p else c3 ]

getWinner' :: ListBoard -> Maybe GameOver
getWinner' (ListBoard rows) = firstJust [ Winner <$> checkRows
                                        , Winner <$> checkCols
                                        , Winner <$> checkDiag 
                                        , checkDrawn ]
  where
    checkRows = firstJust $ map checkRow rows
    checkCols = firstJust $ map (\x -> checkRow $ map (!!x) rows) [0,1,2]
    checkDiag = firstJust [ checkRow [rows !! 0 !! 0, rows !! 1 !! 1, rows !! 2 !! 2]
                          , checkRow [rows !! 2 !! 0, rows !! 1 !! 1, rows !! 0 !! 2] ]
    
    checkDrawn = if Empty `elem` concat rows
                   then Nothing
                   else Just DrawnGame
    
    checkRow :: [Cell] -> Maybe Player
    checkRow = threeSame . playersFromCell
    
    playersFromCell :: [Cell] -> [Player]
    playersFromCell [] = []
    playersFromCell (Empty:rest) = playersFromCell rest
    playersFromCell (PlayerCell p:rest) = p : playersFromCell rest
    
    threeSame :: [Player] -> Maybe Player
    threeSame [p1,p2,p3] = if p1 == p2 && p2 == p3
                             then Just p1
                             else Nothing
    threeSame _ = Nothing
    
    firstJust :: [Maybe a] -> Maybe a
    firstJust (Just a:_) = Just a
    firstJust (_:rest) = firstJust rest
    firstJust [] = Nothing

fromString' :: String -> ListBoard
fromString' str = let (board, rest) = splitAt 11 str
                  in  ListBoard $ map (map readCell) (lines board)

emptyBoard' :: ListBoard
emptyBoard' = ListBoard $ let f = replicate 3 in f (f Empty)

instance Show ListBoard where
  -- show (ListBoard board) = intercalate "\n" $ map (map renderCell) board
  show = prettyPrint

prettyPrint :: ListBoard -> String
prettyPrint (ListBoard rows@[r1,r2,r3]) =
  "  rc 0 1 2" ++ 
  concat (zipWith 
           (\r i -> "\n  " ++ show i ++ "  " ++ unwords (map show r))
           rows [0..]
         )















