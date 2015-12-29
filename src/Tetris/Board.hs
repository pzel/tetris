{-# LANGUAGE RecordWildCards #-}
module Tetris.Board
  (Board(..)
  ,Cell
  ,board
  ,empty
  ,full
  ,overlapsAt
  ,showBoard
  ,spliceBoardAt
  ) where

data Board = Board { boardCells :: [Cell]
                   , boardHeight :: Int
                   , boardWidth :: Int
                   } deriving (Eq,Show)

newtype Cell = Cell { cellValue :: Bool } deriving (Eq,Show)

board :: [[Cell]] -> Board
board cs = Board { boardCells = concat cs
                 , boardHeight = length cs
                 , boardWidth = length (head cs)
                 }

boardRows :: Board -> [[Cell]]
boardRows b = mapChunks id (boardWidth b) (boardCells b)

empty,full :: Cell
empty = Cell False
full = Cell True

showBoard :: Board -> String
showBoard b = concat $ mapChunks showRow (boardWidth b) (boardCells b)
  where
    showRow cs = map showCell cs ++ "\n"
    showCell (Cell True) = '#'
    showCell (Cell False) = '.'

spliceBoardAt :: Board -> (Int, Int) -> Board -> Board
spliceBoardAt b offsetXY child =
  b{boardCells = zipWith cellOr (boardCells b)
                 (paddedTo b offsetXY child)}

overlapsAt :: Board -> (Int, Int) -> Board -> Bool
overlapsAt b offsetXY child =
  let b' = b{boardCells = zipWith cellAnd (boardCells b)
                          (paddedTo b offsetXY child)}
  in any (\c-> cellValue c == True) (boardCells b')

paddedTo :: Board -> (Int, Int) -> Board -> [Cell]
paddedTo parent (x,y) child =
 let rowWidth = boardWidth parent
     emptyRow = take rowWidth (repeat empty)
     paddingFront = take x (repeat empty)
     padRow r = take rowWidth (paddingFront ++ r ++ (repeat empty))
     paddedChildRows = concatMap padRow (boardRows child)
 in concat (take y (repeat emptyRow))
      ++ paddedChildRows
      ++ concat (repeat emptyRow)

(Cell b) `cellOr` (Cell b') = if b || b' then Cell True else Cell False
(Cell b) `cellAnd` (Cell b') = if b && b' then Cell True else Cell False

mapChunks :: ([a] -> [b]) -> Int -> [a] -> [[b]]
mapChunks _ _ [] = []
mapChunks f n l = f front : mapChunks f n back
  where (front, back) = splitAt n l
