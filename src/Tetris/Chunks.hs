module Tetris.Chunks (mapChunks) where

mapChunks :: ([a] -> [b]) -> Int -> [a] -> [[b]]
mapChunks _ _ [] = []
mapChunks f n l = f front : mapChunks f n back
  where (front, back) = splitAt n l
