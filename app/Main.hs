{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Char (chr)
import System.IO
import Tetris

data Game = Game
  { rotation :: Integer
  , block :: Block
  , posX :: Integer
  , posY :: Integer
  , tick :: Integer
  } deriving (Show)

main :: IO ()
main = do
  setupTerm
  hideCursor
  lastKey <- newTVarIO Nothing
  forkIO (inputLoop lastKey)
  mainLoop freshGame lastKey
    where
      setupTerm = hSetBuffering stdin NoBuffering >> hSetEcho stdin False
      freshGame = Game{rotation=0,block=T,posX=0,posY=0,tick=1}


mainLoop :: Game -> TVar (Maybe Char) -> IO ()
mainLoop g inputSource = do
  drawGame g
  threadDelay $ round $ (1000/24.0) * 1000
  i <- getPlayerInput inputSource
  --  if (tick g) > 1000 then error("finished with " ++ show g)
  mainLoop (updateGame i g) inputSource

updateGame :: Maybe Char -> Game  -> Game
updateGame i = mUpdateInput i . updateInvariant
  where
    mUpdateInput Nothing g = g
    mUpdateInput (Just c) g = updateInput c g

updateInvariant :: Game -> Game
updateInvariant g@Game{..} =
  let g' = if tick `mod` 20  == 0
           then g{posY = posY+1, tick = tick+1}
           else g{tick = tick+1}
      g'' = if posY > 15
            then g'{posY = 0, block = T}
            else g'
  in g''

updateInput 'h' g@Game{..} = g{posX = posX-1}
updateInput 'l' g@Game{..} = g{posX = posX+1}
updateInput 'j' g@Game{..} = g{rotation = rotation+1}
updateInput 'k' g@Game{..} = g{rotation = rotation+1}
updateInput 'z' g@Game{..} = g{block = S}
updateInput 'x' g@Game{..} = g{block = Z}
updateInput 'c' g@Game{..} = g{block = I}
updateInput 'v' g@Game{..} = g{block = T}
updateInput 'b' g@Game{..} = g{block = O}
updateInput 'n' g@Game{..} = g{block = L}
updateInput 'm' g@Game{..} = g{block = J}
updateInput _ g = g

drawGame :: Game -> IO ()
drawGame g@Game{..} = do
  clearTerm
  putStrLn $ prefixX posX $ prefixY posY $ draw $ rotate rotation block

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

prefixY :: Integer -> String -> String
prefixY n s = take (fromIntegral n) (repeat '\n') ++ s

prefixX :: Integer -> String -> String
prefixX n = concatMap (\l-> take (fromIntegral n) (repeat ' ') ++ l ++ "\n") . lines
