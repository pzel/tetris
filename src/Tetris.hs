module Tetris
  (Block(..)
  ,BlockDrawing(..)
  ,Pixel(..)
  ,draw
  ,drawBlock
  ,rotate
  ,rotateOnce
  ,composeN
  ) where

data Block = O | L | J | I | T | S | Z deriving (Eq,Show)
data Pixel = On | Of deriving (Eq, Show)
type BlockDrawing = [[Pixel]]

draw :: BlockDrawing -> String
draw b = concatMap drawRow b
    where
      drawRow :: [Pixel] -> String
      drawRow pixels = concatMap drawPixel pixels ++ "\n"
      drawPixel On = "#"
      drawPixel Of = " "

rotate :: Integer -> Block -> BlockDrawing
rotate n b = composeN (normalize n) rotateOnce (drawBlock b) where
  normalize :: (Integral a) => a -> Int
  normalize n = ((fromIntegral n `mod` 4) + 4) `mod` 4

-- Will truncate all to shortest sub-list, like zip
rotateOnce :: (Eq a) => [[a]] -> [[a]]
rotateOnce a = if [] `elem` a then []
           else map last a : rotateOnce (map init a)

composeN :: Int -> (a -> a) -> (a -> a)
composeN n fun = foldr (.) id (replicate n fun)

drawBlock :: Block -> BlockDrawing
drawBlock O = [[On,On], [On,On]]
drawBlock L = [[On,Of], [On,Of], [On,On]]
drawBlock J = [[Of,On], [Of,On], [On,On]]
drawBlock I = [[On], [On], [On], [On]]
drawBlock T = [[On,On,On], [Of,On,Of]]
drawBlock S = [[Of, On, On], [On, On, Of]]
drawBlock Z = [[On, On, Of], [Of, On, On]]
