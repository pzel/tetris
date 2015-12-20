module Tetris
  (Block(..)
  ,BlockDrawing(..)
  ,Rotation(..)
  ,Pixel(..)
  ,draw
  ,drawBlock
  ,mkRotation
  ,rotate
  ) where

import Data.List (transpose)

data Block = O | L | J | I | T | S | Z deriving (Eq,Show)
data Pixel = On | Of deriving (Eq, Show)
type BlockDrawing = [[Pixel]]
data Rotation = Deg0 | Deg90 | Deg180 | Deg270 deriving (Eq,Show)

draw :: BlockDrawing -> String
draw b = concatMap showRow b
    where
      showRow :: [Pixel] -> String
      showRow pixels = concatMap drawPixel pixels ++ "\n"
      drawPixel On = "#"
      drawPixel Of = " "

mkRotation :: (Integral a) => a -> Rotation
mkRotation n = case fromIntegral $ n `mod` 4
               of 0 -> Deg0
                  1 -> Deg90
                  2 -> Deg180
                  3 -> Deg270

rotate :: Rotation -> BlockDrawing -> BlockDrawing
rotate Deg0 b = b
rotate Deg90 b = (transpose . reverse) b
rotate Deg180 b = reverse (map reverse b)
rotate Deg270 b = rotate Deg90 (rotate Deg180 b)

drawBlock :: Block -> BlockDrawing
drawBlock O = [[On,On], [On,On]]
drawBlock L = [[On,Of], [On,Of], [On,On]]
drawBlock J = [[Of,On], [Of,On], [On,On]]
drawBlock I = [[On], [On], [On], [On]]
drawBlock T = [[On,On,On], [Of,On,Of]]
drawBlock S = [[Of, On, On], [On, On, Of]]
drawBlock Z = [[On, On, Of], [Of, On, On]]
