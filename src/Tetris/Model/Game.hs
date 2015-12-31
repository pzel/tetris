module Tetris.Model.Game (Game(..), freshGame, updateGame) where

import Tetris.Model.Board
import Tetris.Controller (InputEvent(..))

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

updateGame :: Game -> InputEvent -> Game
updateGame g@Game{..} ev =
  foldr ($) g [applyInput ev, updateTick, dropBlock, supplyNewBlock]

updateTick :: Game -> Game
updateTick g@Game{..} = g{gameTick = gameTick+1}

supplyNewBlock :: Game -> Game
supplyNewBlock g@Game{gameNewBlockNeeded=True} =
  onLegalGame g{gameNewBlockNeeded=False, gameBlock=pickBlock g, gameBlockY=0, gameBlockX=3}
              id g{gameOver=True}
supplyNewBlock g = g

pickBlock :: Game -> Block
pickBlock g@Game{..} = head (drop n blocks)
  where n = (fromInteger gameTick) + (fromInteger gameBlockRot) + gameBlockX

dropBlock :: Game -> Game
dropBlock g@Game{..} = if gameTick `mod` (dropInterval g) == 0
                       then onLegalGame g{gameBlockY=gameBlockY+1} id (landBlock g)
                       else g

dropInterval :: Game -> Integer
dropInterval g@Game{gameScore=s} = max 2 (8 - (toInteger s `div` 3))

landBlock :: Game -> Game
landBlock g@Game{..} =
  let (n, b') = clearLines (spliceBoardAt gameBoard (gameBlockX,gameBlockY)
                                            (rotated gameBlockRot gameBlock))
  in g{gameBoard=b', gameNewBlockNeeded=True, gameScore=gameScore+n}

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
