module Lib
  ( grid
  , languages
  , outputGrid
  , findWord
  , findWordInLine
  , findWords
  , skewGrid
  , formatGrid
  , coords
  , zipOverGrid
  , zipOverGridWith
  , og
  ) where

import Data
import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

data Cell =
  Cell (Integer, Integer)
       Char
  deriving (Eq, Ord, Show)

type Grid a = [[a]]

outputGrid :: Grid Char -> IO ()
outputGrid grid = putStrLn $ formatGrid grid

og :: Show a => [a] -> IO ()
og = putStrLn . unlines . map show

formatGrid :: Grid Char -> String
formatGrid = unlines

diagonalize :: Grid Char -> Grid Char
diagonalize = transpose . skewGrid

findWords :: Grid Char -> Grid Char -> [String]
findWords grid languages =
  let foundWords = map (findWord grid) languages
   in catMaybes foundWords

getLines :: Grid Char -> Grid Char
getLines grid =
  let horizontal = grid
      vertical = transpose grid
      diagonal1 = diagonalize grid
      diagonal2 = diagonalize (map reverse grid)
      lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
   in lines ++ (map reverse lines)

skewGrid :: Grid Char -> Grid Char
skewGrid [] = []
skewGrid (x:xs) = x : skewGrid (map indent xs)
  where
    indent line = '_' : line

findWord :: Grid Char -> String -> Maybe String
findWord grid word =
  let lines = getLines grid
      found = or $ map (findWordInLine word) lines
   in if found
        then Just word
        else Nothing

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

coordinates :: Grid (Integer, Integer)
coordinates = do
  row <- [0 ..]
  return
    (do coulmn <- [0 ..]
        return (row, coulmn))

coords :: Grid (Integer, Ineger)
coords =
  let row = map repeat [1 ..]
      coulmn = repeat [1 ..]
   in zipOverGrid row coulmn

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith
