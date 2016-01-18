module Tetris.Model.Board
  (Board(..)
  ,Block(..)
  ,BlockType(..)
  ,Cell
  ,board
  ,blocks
  ,clearLines
  ,emptyCell
  ,fullCell
  ,overlapsAt
  ,rotated
  ,spliceBoardAt
  ) where

import Data.List (cycle)
import Data.Function (on)
import Tetris.Chunks (mapChunks)

data Block = Block BlockType deriving (Eq,Show)
data BlockType = I | J | L | O | S | T | Z deriving (Enum,Eq,Ord,Show)
data Board = Board { boardCells :: [Cell]
                   , boardHeight :: Int
                   , boardWidth :: Int
                   } deriving (Eq,Show)

newtype Cell = Cell { cellValue :: Bool } deriving (Eq,Show)

blocks :: [Block]
blocks = map Block (cycle (enumFrom I))

rotated :: Integer -> Block -> Board
rotated n b = composeN (normalize n) rotateOnce (toBoard b) where
  normalize :: (Integral a) => a -> Int
  normalize n = ((fromIntegral n `mod` 4) + 4) `mod` 4

rotateOnce :: Board -> Board
rotateOnce b = board (rotateCells (boardRows b))

rotateCells :: [[Cell]] -> [[Cell]]
rotateCells cs =
  if [] `elem` cs then [] else map last cs : rotateCells (map init cs)

composeN :: Int -> (a -> a) -> (a -> a)
composeN n fun = foldr (.) id (replicate n fun)

toBoard :: Block -> Board
toBoard (Block b) = board (toCells b)
  where
    (e,f) = (emptyCell, fullCell)
    toCells I = [[f], [f], [f], [f]]
    toCells J = [[e,f], [e,f], [f,f]]
    toCells L = [[f,e], [f,e], [f,f]]
    toCells O = [[f,f], [f,f]]
    toCells S = [[e,f,f], [f,f,e]]
    toCells T = [[f,f,f], [e,f,e]]
    toCells Z = [[f,f,e], [e,f,f]]

board :: [[Cell]] -> Board
board cs = Board { boardCells = concat cs
                 , boardHeight = length cs
                 , boardWidth = length (head cs)
                 }

boardRows :: Board -> [[Cell]]
boardRows b = mapChunks id (boardWidth b) (boardCells b)

emptyCell, fullCell :: Cell
emptyCell = Cell False
fullCell = Cell True

clearLines :: Board -> (Int, Board)
clearLines b = clearLines' 0 (boardWidth b) [] (boardRows b)

clearLines' :: Int -> Int -> [[Cell]] -> [[Cell]] -> (Int, Board)
clearLines' n w acc []  = (n, board (pad acc))
  where pad acc = replicate n (replicate w emptyCell) ++ (reverse acc)
clearLines' n w acc (r:rs) = if allFull r
                             then clearLines' (n+1) w acc rs
                             else clearLines' n w (r:acc) rs
  where allFull row = and (map cellValue row)


spliceBoardAt :: Board -> (Int, Int) -> Board -> Board
spliceBoardAt b offsetXY child =
  b{boardCells = zipWith cellOr (boardCells b)
                       (paddedTo b offsetXY child)}

overlapsAt :: Board -> (Int, Int) -> Board -> Bool
overlapsAt b offsetXY child =
  if isLegalOffset b offsetXY child
  then let b' = b{boardCells = zipWith cellAnd (boardCells b)
                            (paddedTo b offsetXY child)}
       in any (\c-> cellValue c == True) (boardCells b')
  else True

isLegalOffset :: Board -> (Int, Int) -> Board -> Bool
isLegalOffset b1 (x,y) b2
  | (x < 0 || y < 0) = False
  | (x + boardWidth b2) > (boardWidth b1) = False
  | (y + boardHeight b2) > (boardHeight b1) = False
  | otherwise = True

paddedTo :: Board -> (Int, Int) -> Board -> [Cell]
paddedTo parent (x,y) child =
 let emptyCells = (repeat emptyCell)
     rowWidth = boardWidth parent
     emptyRow = take rowWidth emptyCells
     paddingFront = take x emptyCells
     padRow r = take rowWidth (paddingFront ++ r ++ emptyCells)
     paddedChildRows = concatMap padRow (boardRows child)
 in concat (take y (repeat emptyRow))
      ++ paddedChildRows
      ++ concat (repeat emptyRow)

cellOr a b = Cell (((||) `on` cellValue) a b)
cellAnd a b = Cell (((&&) `on` cellValue) a b)
