module TypesPresentation where

itoer::String -> String
itoer w = w ++ "ito"

multiGreeter::String -> String -> String
multiGreeter name1 name2 = "Hello" ++ name1 ++ " and " ++ name2

myMap :: (a->b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : map f xs


dummySum::Int -> Int -> Int -> Int
dummySum num1 num2 num3 = num1 + num2 + num3



data Pet = Cat | Dog String
-- "data Pet" is the type definition
-- "Cat | Dog String" is the data definition
-- Cat is a constant value of type Pet
-- Dog is a value that has a string argument


*TypesPresentation> :t Cat
Cat :: Pet
*TypesPresentation> :t Dog "Pluto"
Dog "Pluto" :: Pet
*TypesPresentation> :t Dog "Rintintin"
Dog "Rintintin" :: Pet

