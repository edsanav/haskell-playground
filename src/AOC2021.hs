module AOC2021 where

import Paths_haskell_playground
import qualified Data.Text as T
import Data.List (intercalate)


newtype Checkeable = Checkeable {nums :: [(Int, Bool)]} deriving Show

newtype Panel = Panel {rows::[Checkeable]} deriving Show

cols :: Panel -> [Checkeable]
cols p =
--  let rowIdx row = init [0..length . nums $ row]
-- [[(nums row) !! i | i <- rowIdx]  | row <- rows pan2] 
  [Checkeable [(1,True)]]


readMyContent :: String -> ([Int], [String])
readMyContent fileContent =
  let inputList = map T.unpack (T.splitOn (T.pack "\n\n") (T.pack fileContent))
      numbers = map (read::String -> Int) $ words (head inputList)
  in (numbers, tail inputList)

run :: IO ()
run = do
    file <- getDataFileName "resources/sample.txt"
    mydata <- readFile file
    let (instructions, panelString) = readMyContent mydata
    putStrLn $ "Your data is: " ++ Data.List.intercalate "---" panelString
