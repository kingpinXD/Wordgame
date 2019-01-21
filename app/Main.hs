module Main where

import           Data
import           Lib
import           System.IO
import           System.Random

main :: IO ()
main = do
  gen <- newStdGen
  let filledRandomGrid = fillBlanks gen grid
      game = makeGame filledRandomGrid languages
  hSetBuffering stdout NoBuffering
  playTurn game

playTurn game = do
  putStrLn . formatGame $ game
  putStr "Please enter a word :"
  word <- getLine
  let newgame = playGame game word
  if newgame == game
    then putStr "The word does no exist in grid \n"
    else putStr "Correct \n"
  if completed newgame
    then putStr "Congratulations\n"
    else playTurn newgame
