module Tetris.View (showGame) where
import Tetris.Model.Game (Game(..))
import Tetris.Model.Board (rotated, showBoard, spliceBoardAt)

showGame :: Game -> String
showGame g@Game{..} =
  showBoard $ spliceBoardAt gameBoard
              (gameBlockX, gameBlockY)
              (rotated gameBlockRot gameBlock)
