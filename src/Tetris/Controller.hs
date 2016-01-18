module Tetris.Controller
  (InputSource
  ,getInputEvent
  ,startInputLoop
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (when)
import Data.Maybe (isJust, maybe)
import Tetris.Model.Game (InputEvent(..))

data InputSource = InputSource (TVar (Maybe Char))

startInputLoop :: IO InputSource
startInputLoop = do
  is <- InputSource <$> newTVarIO Nothing
  forkIO (inputLoop is)
  return is

getInputEvent :: InputSource -> IO InputEvent
getInputEvent (InputSource tvar) = do
  tick
  c <- readTVarIO tvar
  when (isJust c) (atomically (writeTVar tvar Nothing))
  return (inputEvent c)

inputLoop :: InputSource -> IO ()
inputLoop i@(InputSource tvar) =
  getChar >>= atomically . writeTVar tvar . Just >> inputLoop i

inputEvent :: Maybe Char -> InputEvent
inputEvent = maybe NoInput charToInput

charToInput :: Char -> InputEvent
charToInput c
  | c `elem` "ah" = MoveLeft
  | c `elem` "dl" = MoveRight
  | c `elem` "sj" = RotateCC
  | c `elem` "wk" = RotateC
  | c == ' '      = Drop
  | otherwise     = NoInput

tick :: IO ()
tick = threadDelay $ (1000 `div` 12) * 1000
