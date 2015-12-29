module Tetris.Board
  (Board(..)
  ,Cell
  ,board
  ,empty
  ,full
  ,offsetPairs
  ,showBoard
  ,spliceBoardAt
  ) where

import Data.Array.IArray (Array, (//), bounds, elems, listArray)
import Data.List ((!!), splitAt)

newtype Board = Board { boardCells :: Array (Int,Int) Cell } deriving (Eq,Show)
data Cell = Full | Empty deriving (Eq,Show)

board :: [[Cell]] -> Board
board cs = Board $ listArray ((1,1),(height,width)) (concat cs)
  where width = length (head cs)
        height = length cs

empty,full :: Cell
empty = Empty
full = Full

showBoard :: Board -> String
showBoard b = mapChunks showRow (rowLength b) (allCells b)
  where
    allCells = elems . boardCells
    rowLength = snd . snd . bounds . boardCells
    showRow cs = map showCell cs ++ "\n"
    showCell Full = '#'
    showCell Empty = ' '

spliceBoardAt :: Board -> (Int, Int) -> [[Cell]] -> Board
spliceBoardAt parent offsetXY child =
  Board $ (boardCells parent) // offsetPairs offsetXY child

mapChunks :: ([a] -> [b]) -> Int -> [a] -> [b]
mapChunks _ _ [] = []
mapChunks f n l = f front ++ mapChunks f n back
  where (front, back) = splitAt n l

offsetPairs :: (Int, Int) -> [[a]] -> [((Int, Int), a)]
offsetPairs (offsetX,offsetY) l =
  let (width,height) = (length (head l), length l)
  in [((y+offsetY, x+offsetX), (l !! y) !! x)
      | x <- [0..width-1], y <- [0..height-1]]
