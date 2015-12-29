module Tetris.Game
  (Game(..))
  ,updateGame
  ) where

import Tetris.Board

data Game = Game
  { board :: Board
  , currentBlock :: Block
  , ticks :: Integer
  , alive :: Bool
  } deriving (Eq, Show, Ord)


