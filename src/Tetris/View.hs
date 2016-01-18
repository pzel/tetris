module Tetris.View (showGame) where
import Tetris.Chunks (mapChunks)
import Tetris.Model.Game (Game(..))
import Tetris.Model.Board (Board(..), full, rotated, spliceBoardAt)

showGame :: Game -> String
showGame g@Game{..} =
  showBoard $ spliceBoardAt gameBoard
              (gameBlockX, gameBlockY)
              (rotated gameBlockRot gameBlock)

showBoard :: Board -> String
showBoard b@Board{..} = concat $ mapChunks showRow boardWidth boardCells
  where
    showRow cs = map showCell cs ++ "\n"
    showCell c = if c == full then  '\9608' else '.'
