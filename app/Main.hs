{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Char (chr)
import System.IO
import Tetris.Game

main :: IO ()
main = do
  setupTerm
  hideCursor
  lastKey <- newTVarIO Nothing
  forkIO (inputLoop lastKey)
  mainLoop freshGame lastKey
    where
      setupTerm = hSetBuffering stdin NoBuffering >> hSetEcho stdin False

mainLoop :: Game -> TVar (Maybe Char) -> IO ()
mainLoop g inputSource = do
  drawGame g
  threadDelay $ round $ (1000/12.0) * 1000
  i <- getPlayerInput inputSource
  mainLoop (updateGame g (inputEvent i)) inputSource

drawGame :: Game -> IO ()
drawGame g@Game{..} = clearTerm >> putStrLn (showGame g)

getPlayerInput :: TVar (Maybe Char) -> IO (Maybe Char)
getPlayerInput inputSource = do
  c <- readTVarIO inputSource
  case c of
    Just _ -> atomically (writeTVar inputSource Nothing) >> return c
    Nothing -> return c

inputLoop :: TVar (Maybe Char) -> IO ()
inputLoop input =
  getChar >>= atomically . writeTVar input . Just >> inputLoop input

clearTerm, hideCursor, homeCursor :: IO ()
clearTerm = putStrLn (chr 27 : "[2J") >> homeCursor
homeCursor = putStr (chr 27 : "[H")
hideCursor = putStr (chr 27 : "[?25l")
