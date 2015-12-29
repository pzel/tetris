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

data InputEvent = MoveLeft | MoveRight | RotateCC | RotateC | NoInput
                  deriving (Eq,Show)
w = 12
h = 17

inputEvent :: Maybe Char -> InputEvent
inputEvent (Just 'h') = MoveLeft
inputEvent (Just 'l') = MoveRight
inputEvent (Just 'j') = RotateCC
inputEvent (Just 'k') = RotateC
inputEvent _ = NoInput

freshGame :: Int -> Game
freshGame randomSeed = Game
            { gameBoard = board (take h (repeat (take w (repeat empty))))
            , gameBlock = head (drop (randomSeed `mod` 13) blocks)
            , gameBlockX = 3
            , gameBlockY = 0
            , gameBlockRot = 0
            , gameTick = 1
            , gameScore = 0
            , gameOver = False
            , gameNewBlockNeeded = False
            }

showGame :: Game -> String
showGame g@Game{..} =
  showBoard $ spliceBoardAt gameBoard (gameBlockX, gameBlockY)
              (rotated gameBlockRot gameBlock)

updateGame :: Game -> InputEvent -> Game
updateGame g@Game{..} input =
  maybeSupplyNewBlock
  $ maybeDropBlock
  $ updateTick
  $ maybeMove input g

updateTick :: Game -> Game
updateTick g@Game{..} = g{gameTick = gameTick+1}

maybeSupplyNewBlock :: Game -> Game
maybeSupplyNewBlock g@Game{gameNewBlockNeeded=True} =
  g{gameNewBlockNeeded=False
   ,gameBlock = pseudoRandomBlock g
   ,gameBlockY = 0
   ,gameBlockX = 3
   }
maybeSupplyNewBlock g = g

pseudoRandomBlock :: Game -> Block
pseudoRandomBlock g@Game{..} = head (drop n blocks)
  where
    n = (fromInteger gameTick) + (fromInteger gameBlockRot) + gameBlockX

maybeDropBlock :: Game -> Game
maybeDropBlock g@Game{..} =
  if gameTick `mod` (dropInterval g) == 0
  then
    if not $ overlapsAt gameBoard (gameBlockX,gameBlockY + 1)
         (rotated gameBlockRot gameBlock)
    then g{gameBlockY = gameBlockY + 1}
    else dropBlock g
  else g

dropInterval :: Game -> Integer
dropInterval g@Game{gameScore=s} = max 1 (8 - (toInteger s))

dropBlock :: Game -> Game
dropBlock g@Game{..} =
  let b = spliceBoardAt gameBoard (gameBlockX,gameBlockY)
                                  (rotated gameBlockRot gameBlock)
      (n, b') = clearLines b
      topRowTaken = gameBlockY <= 1
  in if topRowTaken
     then g{gameOver = True
           ,gameBoard = b'
           }
     else g{gameBoard = b'
           ,gameNewBlockNeeded = True
           ,gameScore = gameScore+n
           }

maybeClearLines :: Game -> Game
maybeClearLines g@Game{..} = g{gameBoard = snd (clearLines gameBoard)}

maybeMove :: InputEvent -> Game -> Game
maybeMove MoveLeft g@Game{..} =
  let newX = gameBlockX-1
      rb = rotated gameBlockRot gameBlock
  in onLegalMove g (newX,gameBlockY) rb g{gameBlockX = newX}
maybeMove MoveRight g@Game{..} =
  let newX = gameBlockX+1
      rb = rotated gameBlockRot gameBlock
  in onLegalMove g (newX,gameBlockY) rb g{gameBlockX = newX}
maybeMove RotateCC g@Game{..} =
  let newRot = gameBlockRot+1
      rb = rotated newRot gameBlock
  in onLegalMove g (gameBlockX,gameBlockY) rb g{gameBlockRot = newRot}
maybeMove RotateC g@Game{..} =
  let newRot = gameBlockRot-1
      rb = rotated newRot gameBlock
  in onLegalMove g (gameBlockX,gameBlockY) rb g{gameBlockRot = newRot}
maybeMove _ g = g

onLegalMove :: Game -> (Int, Int) -> Board -> Game -> Game
onLegalMove g@Game{..} (x,y) nb ng =
  if not $ overlapsAt gameBoard (x,y) nb then ng else g
