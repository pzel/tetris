module Main where

import Control.Concurrent (threadDelay)
import Data.Char (chr)
import System.CPUTime (getCPUTime)
import System.Process (system)
import Tetris

moveToHome :: IO ()
moveToHome = putStrLn [chr 27, '[', 'H']

clearLine :: IO ()
clearLine = putStrLn  [chr 27, '[', 'K']

main :: IO ()
main = system "stty -echo; clear" >> loop 0

loop r = do
  threadDelay $ round (1000*900)
  moveToHome >> sequence_ (take 20 (repeat clearLine))  >> moveToHome
  putStrLn (draw (rotate r T))
  loop (r+1)
