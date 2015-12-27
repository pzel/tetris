module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Char (chr)
import System.IO
import Tetris

setupTerm :: IO ()
setupTerm =
  hSetBuffering stdin NoBuffering >> hSetEcho stdin False

clearTerm :: IO ()
clearTerm =
  moveToHome >> sequence_ (take 20 (repeat clearLine)) >> moveToHome
    where
      clearLine = putStrLn  [chr 27, '[', 'K']
      moveToHome = putStrLn [chr 27, '[', 'H']

main :: IO ()
main = do
  setupTerm
  lastKey <- newTVarIO Nothing
  forkIO (inputLoop lastKey)
  displayLoop lastKey 0

displayLoop :: TVar (Maybe Char) -> Integer -> IO ()
displayLoop input r = do
  threadDelay $ round $ (1000/10.0) * 1000
  clearTerm
  x <- readTVarIO input
  case x of
    Just _ -> do
           putStrLn (draw (rotate r T))
           atomically (writeTVar input Nothing)
           displayLoop input (r+1)
    Nothing -> displayLoop input (r+1)

inputLoop :: TVar (Maybe Char) -> IO ()
inputLoop input =
  getChar >>= atomically . writeTVar input . Just >> inputLoop input
