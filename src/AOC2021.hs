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
  
byCols :: Panel -> Panel
byCols p = 
   let  withCol = [zip [0..] row | row <- p] :: [[(Int, (Int, Bool))]]
        -- Here on applies == to the result of applying first to the two elements it's comparing
        grouped = groupBy ((==) `on` fst)  $ concat withCol
   in [[snd x | x <-xs] | xs <- grouped]

unchecked :: Panel -> [Int]
unchecked p = map fst $ filter (not . snd) $ concat p

draw :: Int -> Panel -> Panel
draw n p =
    let mark (x, b) = if x==n then (x, True) else (x, b)
    in [map mark row | row <- p ]


readMyContent :: String -> ([Int], [Panel])
readMyContent fileContent =
  let inputList = map T.unpack (T.splitOn (T.pack "\n\n") (T.pack fileContent))
      readNumsLine inp = map (read::String -> Int) (words inp)
      numbers = readNumsLine (head inputList)
--      readPanel pStr = map (\x -> (x, False)) $ map readNumsLine $ lines pStr
      readPanel pStr = map (map (, False) . readNumsLine) (lines pStr)
  in (numbers, map readPanel (tail inputList))

-- TODO multiply by last number

firstWin :: [Panel] -> Int -> ([Panel], Maybe Int)
firstWin ps n = (nps, result $ filter isWinner nps)
        where 
          nps = map (draw n) ps
          result [x] = Just (sum $ unchecked x)
          result _ = Nothing
          
lastWin :: [Panel] -> Int -> ([Panel], Maybe Int)
lastWin ps n = 
      if any isWinner nps
      then (nps, Nothing)
      else (nps, result $ filter (not. isWinner) ps)
      where 
          nps = map (draw n) ps
          result [x] = Just (sum (unchecked (draw n x)))
          result _ = Nothing
             

run :: IO ()
run = do
    file <- getDataFileName "resources/sample.txt"
    mydata <- readFile file
    let (instructions, panelString) = readMyContent mydata
    putStrLn $ "Your data is: " ++ Data.List.intercalate "---" (map show panelString)
