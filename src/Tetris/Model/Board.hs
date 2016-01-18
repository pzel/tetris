module Tetris.Model.Board
  (Board(..)
  ,Block(..)
  ,BlockType(..)
  ,Cell
  ,board
  ,blocks
  ,clearLines
  ,empty
  ,full
  ,overlapsAt
  ,rotated
  ,showBoard
  ,spliceBoardAt
  ) where

import Data.List (cycle)
import Data.Function (on)

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
    toCells I = [[full], [full], [full], [full]]
    toCells J = [[empty,full], [empty,full], [full,full]]
    toCells L = [[full,empty], [full,empty], [full,full]]
    toCells O = [[full,full], [full,full]]
    toCells S = [[empty, full, full], [full, full, empty]]
    toCells T = [[full,full,full], [empty,full,empty]]
    toCells Z = [[full, full, empty], [empty, full, full]]

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

clearLines :: Board -> (Int, Board)
clearLines b = clearLines' 0 (boardWidth b) [] (boardRows b)

clearLines' :: Int -> Int -> [[Cell]] -> [[Cell]] -> (Int, Board)
clearLines' n w acc []  = (n, board (pad acc))
  where pad acc = replicate n (replicate w empty) ++ (reverse acc)
clearLines' n w acc (r:rs) = if allFull r
                             then clearLines' (n+1) w acc rs
                             else clearLines' n w (r:acc) rs
  where allFull row = and (map cellValue row)

showBoard :: Board -> String
showBoard b = concat $ mapChunks showRow (boardWidth b) (boardCells b)
  where
    showRow cs = map showCell cs ++ "\n"
    showCell (Cell True) = '\9608'
    showCell (Cell False) = '.'

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
 let rowWidth = boardWidth parent
     emptyRow = take rowWidth (repeat empty)
     paddingFront = take x (repeat empty)
     padRow r = take rowWidth (paddingFront ++ r ++ (repeat empty))
     paddedChildRows = concatMap padRow (boardRows child)
 in concat (take y (repeat emptyRow))
      ++ paddedChildRows
      ++ concat (repeat emptyRow)

cellOr a b = Cell (((||) `on` cellValue) a b)
cellAnd a b = Cell (((&&) `on` cellValue) a b)

mapChunks :: ([a] -> [b]) -> Int -> [a] -> [[b]]
mapChunks _ _ [] = []
mapChunks f n l = f front : mapChunks f n back
  where (front, back) = splitAt n l