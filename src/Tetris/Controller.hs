module Tetris.Controller
  (InputEvent(..)
  ,InputSource
  ,getInputEvent
  ,startInputLoop
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (when)
import Data.Maybe (isJust)

data InputSource = InputSource (TVar (Maybe Char))
data InputEvent = MoveLeft | MoveRight
                | RotateCC | RotateC
                | Drop | NoInput
                  deriving (Eq,Show)

startInputLoop :: IO InputSource
startInputLoop = do
  is <- fmap InputSource (newTVarIO Nothing)
  forkIO (inputLoop is)
  return is

getInputEvent :: InputSource -> IO InputEvent
getInputEvent (InputSource tvar) = do
  tick
  c <- readTVarIO tvar
  when (isJust c) (atomically (writeTVar tvar Nothing))
  return (inputEvent c)

inputLoop :: InputSource -> IO ()
inputLoop i@(InputSource tvar) = getChar >>= atomically . writeTVar tvar . Just >> inputLoop i

inputEvent :: Maybe Char -> InputEvent
inputEvent (Just 'h') = MoveLeft
inputEvent (Just 'l') = MoveRight
inputEvent (Just 'j') = RotateCC
inputEvent (Just 'k') = RotateC
inputEvent (Just ' ') = Drop
inputEvent _ = NoInput


tick :: IO ()
tick = threadDelay $ (1000 `div` 12) * 1000
