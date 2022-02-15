module AOC2021 where

import Paths_haskell_playground
import qualified Data.Text as T
import Data.List (intercalate, groupBy)
import Data.Function (on)


type Checkeable = [(Int, Bool)]

type Panel = [Checkeable]

isLine :: Checkeable -> Bool
isLine = all snd

isWinner :: Panel -> Bool
isWinner p = (any isLine p) -- || by cols

getCol :: Int -> Panel -> Checkeable
getCol i p = 
  let withCol = [zip [0..] row | row <- p]
      cols = filter (\x -> fst x == i) (concat withCol)
  in map snd cols
  
byCols :: Panel -> Panel
byCols p = 
   let  withCol = [zip [0..] row | row <- p] :: [[(Int, (Int, Bool))]]
        grouped = groupBy ((==) `on` fst)  $ concat withCol
   in [[snd x | x <-xs] | xs <- grouped]
  

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
