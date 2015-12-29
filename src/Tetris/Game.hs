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
-- import Tetris.Block (Block(..))

data Game = Game
  { gameBoard :: Board
  , gameBlock :: Block
  , gameBlockX :: Int
  , gameBlockY :: Int
  , gameBlockRot :: Integer
  , gameTick :: Integer
  , gameAlive :: Bool
  } deriving (Eq, Show)

data InputEvent = MoveLeft | MoveRight
                | RotateCC | RotateC
                | NoInput
                  deriving (Eq,Show)

w = 10
h = 20
speed = 5

inputEvent :: Maybe Char -> InputEvent
inputEvent (Just 'h') = MoveLeft
inputEvent (Just 'l') = MoveRight
inputEvent (Just 'j') = RotateCC
inputEvent (Just 'k') = RotateC
inputEvent _ = NoInput

freshGame :: Game
freshGame = Game
            { gameBoard = board (take h (repeat (take w (repeat empty))))
            , gameBlock = (Block T)
            , gameBlockX = 1
            , gameBlockY = 0
            , gameBlockRot = 0
            , gameTick = 1
            , gameAlive = True }

showGame :: Game -> String
showGame g@Game{..} =
  showBoard $ spliceBoardAt gameBoard (gameBlockX, gameBlockY)
              (rotated gameBlockRot gameBlock)

updateGame :: Game -> InputEvent -> Game
updateGame g@Game{..} input =
   maybeDropBlock . updateTick . (maybeMove input) $ g

updateTick :: Game -> Game
updateTick g@Game{..} = g{gameTick = gameTick+1}

maybeDropBlock :: Game -> Game
maybeDropBlock g@Game{..} =
  if gameTick `mod` speed == 0
  then
    if not $ overlapsAt gameBoard (gameBlockX,gameBlockY + 1)
         (rotated gameBlockRot gameBlock)
    then g{gameBlockY = gameBlockY + 1}
    else g{gameBoard = spliceBoardAt gameBoard
                                    (gameBlockX,gameBlockY)
                                    (rotated gameBlockRot gameBlock)
          ,gameBlockY = 0
          }
  else g

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
