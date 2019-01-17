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
  , gridWithCoords
  , findWordsChar
  , findWordsCoords
  ) where

import           Data
import           Data.List  (isInfixOf, transpose)
import           Data.Maybe (catMaybes, listToMaybe)

data Cell
  = Cell (Integer, Integer)
         Char
  | Indent
  deriving (Eq, Ord, Show)

type Grid a = [[a]]

outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn $ formatGrid grid

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

og :: Show a => [a] -> IO ()
og = putStrLn . unlines . map show

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cellToChar

cellToChar :: Cell -> Char
cellToChar (Cell _ c) = c
cellToChar Indent     = '?'

cellToCoords :: Cell -> (Integer, Integer)
cellToCoords (Cell (x, y) _) = (x, y)
cellToCoords Indent          = (0, 0)

diagonalize :: Grid Cell -> Grid Cell
diagonalize = transpose . skewGrid

getLines :: Grid Cell -> [[Cell]]
getLines grid =
  let horizontal = grid
      vertical = transpose grid
      diagonal1 = diagonalize grid
      diagonal2 = diagonalize (map reverse grid)
      lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
   in lines ++ (map reverse lines)

skewGrid :: Grid Cell -> Grid Cell
skewGrid [] = []
skewGrid (x:xs) = x : skewGrid (map indent xs)
  where
    indent line = Indent : line

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
  let lines = getLines grid
      found = map (findWordInLine word) lines
   in listToMaybe (catMaybes found)

findWords :: Grid Cell -> Grid Char -> [[Cell]]
findWords grid languages =
  let foundWords = map (findWord grid) languages
   in catMaybes foundWords

findWordsChar :: Grid Char -> Grid Char -> [String]
findWordsChar grid languages =
  mapOverGrid cellToChar (findWords (gridWithCoords grid) languages)

findWordsCoords :: Grid Char -> Grid Char -> Grid (Integer, Integer)
findWordsCoords grid languages =
  mapOverGrid cellToCoords (findWords (gridWithCoords grid) languages)

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word cellline =
  let found = findWordInLinePrefix [] word cellline
   in case found of
        Nothing     -> findWordInLine word (tail cellline)
        cs@(Just _) -> cs

findWordInLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInLinePrefix acc (x:xs) (c:cs)
  | x == cellToChar c = findWordInLinePrefix (c : acc) xs cs
findWordInLinePrefix acc [] _ = Just $ reverse acc
findWordInLinePrefix _ _ _ = Nothing

coordinates :: Grid (Integer, Integer)
coordinates = do
  row <- [0 ..]
  return
    (do coulmn <- [0 ..]
        return (row, coulmn))

coords :: Grid (Integer, Integer)
coords =
  let row = map repeat [1 ..]
      coulmn = repeat [1 ..]
   in zipOverGrid row coulmn

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords = zipOverGridWith Cell coords
