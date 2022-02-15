{-# LANGUAGE TupleSections #-}

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
isWinner p = any isLine p || any isLine (byCols p)

getCol :: Int -> Panel -> Checkeable
getCol i p = 
  let withCol = [zip [0..] row | row <- p]
      cols = filter (\x -> fst x == i) (concat withCol)
  in map snd cols
  
byCols :: Panel -> Panel
byCols p = 
   let  withCol = [zip [0..] row | row <- p] :: [[(Int, (Int, Bool))]]
        -- Here on applies == to the result of applying first to the two elements it's comparing
        grouped = groupBy ((==) `on` fst)  $ concat withCol
   in [[snd x | x <-xs] | xs <- grouped]  

readMyContent :: String -> ([Int], [Panel])
readMyContent fileContent =
  let inputList = map T.unpack (T.splitOn (T.pack "\n\n") (T.pack fileContent))
      readNumsLine inp = map (read::String -> Int) (words inp)
      numbers = readNumsLine (head inputList)
--      readPanel pStr = map (\x -> (x, False)) $ map readNumsLine $ lines pStr
      readPanel pStr = map (map (, False) . readNumsLine) (lines pStr)
  in (numbers, map readPanel (tail inputList))

run :: IO ()
run = do
    file <- getDataFileName "resources/sample.txt"
    mydata <- readFile file
    let (instructions, panelString) = readMyContent mydata
    putStrLn $ "Your data is: " ++ Data.List.intercalate "---" (map show panelString)
