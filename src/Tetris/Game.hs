{-# LANGUAGE RecordWildCards #-}
module Tetris.Game
  (Game(..)
  ,InputEvent
  ,freshGame
  ,inputEvent
  ,showGame
  ,updateGame
  ) where

import Tetris.Board

data Game = Game
  { gameBoard :: Board
  , gameBlock :: Block
  , gameBlockX :: Int
  , gameBlockY :: Int
  , gameBlockRot :: Integer
  , gameTick :: Integer
  , gameScore :: Int
  , gameOver :: Bool
  , gameNewBlockNeeded :: Bool
  } deriving (Eq, Show)

data InputEvent = MoveLeft | MoveRight | RotateCC | RotateC | Drop | NoInput
                  deriving (Eq,Show)
inputEvent :: Maybe Char -> InputEvent
inputEvent (Just 'h') = MoveLeft
inputEvent (Just 'l') = MoveRight
inputEvent (Just 'j') = RotateCC
inputEvent (Just 'k') = RotateC
inputEvent (Just ' ') = Drop
inputEvent _ = NoInput

freshGame :: Int -> Game
freshGame randomSeed =
  Game { gameBoard = defaultBoard
       , gameBlock = head (drop (randomSeed `mod` 7) blocks)
       , gameBlockX = 3, gameBlockY = 0
       , gameBlockRot = 0, gameTick = 1
       , gameScore = 0, gameOver = False
       , gameNewBlockNeeded = False}
    where cells = repeat empty
          rows = repeat (take defaultWidth cells)
          defaultBoard = board $ take defaultHeight rows
          (defaultWidth, defaultHeight) = (10,20)

showGame :: Game -> String
showGame g@Game{..} =
  showBoard $ spliceBoardAt gameBoard (gameBlockX, gameBlockY)
              (rotated gameBlockRot gameBlock)

updateGame :: Game -> InputEvent -> Game
updateGame g@Game{..} ev =
  foldr ($) g [applyInput ev, updateTick, dropBlock, supplyNewBlock]

updateTick :: Game -> Game
updateTick g@Game{..} = g{gameTick = gameTick+1}

supplyNewBlock :: Game -> Game
supplyNewBlock g@Game{gameNewBlockNeeded=True} =
  g{gameNewBlockNeeded=False, gameBlock=pickBlock g, gameBlockY=0, gameBlockX=3}
supplyNewBlock g = g

pickBlock :: Game -> Block
pickBlock g@Game{..} = head (drop n blocks)
  where n = (fromInteger gameTick) + (fromInteger gameBlockRot) + gameBlockX

dropBlock :: Game -> Game
dropBlock g@Game{..} =
  if gameTick `mod` (dropInterval g) == 0
  then if not $ overlapsAt gameBoard (gameBlockX,gameBlockY + 1)
         (rotated gameBlockRot gameBlock)
       then g{gameBlockY = gameBlockY + 1}
       else landBlock g
  else g

dropInterval :: Game -> Integer
dropInterval g@Game{gameScore=s} = max 1 (8 - (toInteger s `mod` 2))

landBlock :: Game -> Game
landBlock g@Game{..} =
  let b = spliceBoardAt gameBoard (gameBlockX,gameBlockY)
                                  (rotated gameBlockRot gameBlock)
      (n, b') = clearLines b
      topRowTaken = gameBlockY <= 1
  in if topRowTaken
     then g{gameOver=True, gameBoard = b'}
     else g{gameBoard=b', gameNewBlockNeeded=True, gameScore=gameScore+n}

applyInput :: InputEvent -> Game -> Game
applyInput MoveLeft g@Game{..} = onLegalGame g{gameBlockX=gameBlockX-1} id g
applyInput MoveRight g@Game{..} = onLegalGame g{gameBlockX=gameBlockX+1} id g
applyInput RotateCC g@Game{..} = onLegalGame g{gameBlockRot=gameBlockRot+1} id g
applyInput RotateC g@Game{..} = onLegalGame g{gameBlockRot=gameBlockRot-1} id g
applyInput Drop g@Game{..} = onLegalGame g{gameBlockY=gameBlockY+1} (applyInput Drop) g
applyInput _ g = g

onLegalGame :: Game -> (Game -> Game) -> Game -> Game
onLegalGame ng f def = if isLegalGame ng then f ng else def

isLegalGame :: Game -> Bool
isLegalGame g@Game{..} =
  not $ overlapsAt gameBoard (gameBlockX,gameBlockY) (rotated gameBlockRot gameBlock)

