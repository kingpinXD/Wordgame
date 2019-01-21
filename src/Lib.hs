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
  , makeGame
  , totalWords
  , score
  , formatGame
  , playGame
  , completed
  , makeRandomGrid
  , fillBlanks
  ) where

import           Data
import           Data.Char     (toLower)
import           Data.List     (isInfixOf, transpose)
import qualified Data.Map      as M
import           Data.Maybe    (catMaybes, listToMaybe)
import           System.Random

data Game = Game
  { gameGrid  :: Grid Cell
  , gameWords :: M.Map String (Maybe [Cell])
  } deriving (Eq, Ord, Show)

data Cell
  = Cell (Integer, Integer)
         Char
  | Indent
  deriving (Eq, Ord, Show)

type Grid a = [[a]]

completed :: Game -> Bool
completed game = score game == totalWords game

makeRandomGrid :: RandomGen t => t -> Grid Char
makeRandomGrid gen =
  let (gen1, gen2) = split gen
      row = randomRs ('A', 'Z') gen1
   in row : makeRandomGrid gen2

fillBlanks :: RandomGen t => t -> Grid Char -> Grid Char
fillBlanks gen grid =
  let randomGrid = makeRandomGrid gen
      fill '_' r = r
      fill c _   = c
   in zipOverGridWith fill grid randomGrid

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let gamegrid = gridWithCoords grid
      tuplify word = (word, Nothing)
      wordtuple = map tuplify words
      wordsdict = M.fromList wordtuple
   in Game gamegrid wordsdict

formatGameGrid :: Game -> String
formatGameGrid game =
  let grid = gameGrid game
      dict = gameWords game
      cellSet = concat . catMaybes . M.elems $ dict
      formatcell cell =
        let charofcell = cellToChar cell
         in if cell `elem` cellSet
              then charofcell
              else toLower charofcell
      chargrid = mapOverGrid formatcell grid
   in unlines chargrid

playGame :: Game -> String -> Game
playGame game word
  | not $ M.member word (gameWords game) = game
playGame game word =
  let grid = gameGrid game
      found = findWord grid word
   in case found of
        Nothing -> game
        Just cs ->
          let dict = gameWords game
              newDict = M.insert word found dict
           in game {gameWords = newDict}

formatGame :: Game -> String
formatGame game =
  let formatedgame = formatGameGrid game
   in formatedgame ++
      "\n\n" ++ (show $ score game) ++ "/" ++ (show $ totalWords game)

totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game

score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game

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
