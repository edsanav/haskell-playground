module AOC2021 where

import Paths_haskell_playground

run :: IO ()
run = do
    file <- getDataFileName "resources/sample.txt"
    mydata <- readFile file
    putStrLn $ "Your data is: " ++ mydata
