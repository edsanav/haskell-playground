module AOC2021 where

import Paths_haskell_playground
import qualified Data.Text as T
import Data.List (intercalate)


type Checkeable = [(Int, Bool)]

type Panel = [Checkeable]

isLine :: Checkeable -> Bool
isLine = all snd

getCol :: Int -> Panel -> Checkeable
getCol i p = 
  let withCol = [zip [0..] row | row <- p]
      cols = filter (\x -> fst x == i) (concat withCol)
  in map snd cols

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
