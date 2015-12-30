{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Char (chr)
import System.CPUTime (getCPUTime)
import System.IO
import Tetris.Game

main :: IO ()
main = do
  setupTerm
  seed <- fmap fromInteger getCPUTime
  lastKey <- newTVarIO Nothing
  forkIO (inputLoop lastKey)
  mainLoop (freshGame seed) lastKey
 where
   setupTerm = hSetBuffering stdin NoBuffering >> hSetEcho stdin False >> hideCursor

mainLoop :: Game -> TVar (Maybe Char) -> IO ()
mainLoop g@Game{gameOver=False} inputSource = do
  drawGame g
  threadDelay $ (1000 `div` 12) * 1000
  i <- getPlayerInput inputSource
  mainLoop (updateGame g (inputEvent i)) inputSource
mainLoop g@Game{gameScore=s} _ = do
  drawGame g
  putStrLn $ "You cleared: " ++ (show s) ++ " lines."

inputLoop :: TVar (Maybe Char) -> IO ()
inputLoop input = getChar >>= atomically . writeTVar input . Just >> inputLoop input

drawGame :: Game -> IO ()
drawGame g@Game{..} = clearTerm >> putStrLn (showGame g)

getPlayerInput :: TVar (Maybe Char) -> IO (Maybe Char)
getPlayerInput inputSource = do
  c <- readTVarIO inputSource
  case c of
    Just _ -> atomically (writeTVar inputSource Nothing) >> return c
    Nothing -> return c

clearTerm, hideCursor, homeCursor :: IO ()
clearTerm = putStrLn (chr 27 : "[2J") >> homeCursor
homeCursor = putStr (chr 27 : "[H")
hideCursor = putStr (chr 27 : "[?25l")
