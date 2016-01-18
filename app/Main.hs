module Main where

import Data.Char (chr)
import System.CPUTime (getCPUTime)
import System.IO
import Tetris (Game(..), InputSource, freshGame
              ,getInputEvent, startInputLoop, updateGame, showGame)

main :: IO ()
main = do
  setupTerm
  inputSource <- startInputLoop
  seed <- fmap fromInteger getCPUTime
  mainLoop (freshGame seed) inputSource
 where
   setupTerm = hSetBuffering stdin NoBuffering >>
               hSetEcho stdin False >> hideCursor

mainLoop :: Game -> InputSource -> IO ()
mainLoop g@Game{gameOver=False} is =
  drawGame g >> getInputEvent is >>= \i -> mainLoop (updateGame i g) is
mainLoop g@Game{gameScore=s} _ =
  drawGame g >> (putStrLn $ "You cleared: " ++ (show s) ++ " lines.") >>
  showCursor

drawGame :: Game -> IO ()
drawGame g = clearTerm >> putStr (showGame g)

clearTerm, hideCursor, homeCursor, showCursor :: IO ()
clearTerm = putStrLn (chr 27 : "[2J") >> homeCursor
homeCursor = putStr (chr 27 : "[H")
hideCursor = putStr (chr 27 : "[?25l")
showCursor = putStr (chr 27 : "[?25h")
