module Scratch where

bla :: Integer -> Integer -> Integer
bla x y=  x + y

getlength :: [Char] -> Integer
getlength xs = sum [1 | _ <- xs]

main :: IO ()
main = do
  putStrLn "Scratching...."
