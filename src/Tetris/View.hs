{-# LANGUAGE RecordWildCards #-}
module Tetris.View (showGame) where

import Tetris.Game (Game(..))
import Tetris.Board (rotated, showBoard, spliceBoardAt)

showGame :: Game -> String
showGame g@Game{..} =
  showBoard $ spliceBoardAt gameBoard
              (gameBlockX, gameBlockY)
              (rotated gameBlockRot gameBlock)
