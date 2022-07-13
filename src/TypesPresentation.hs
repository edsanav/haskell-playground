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



--data SpecialBool = True | False

type Coord = Double
data Point = Point Coord Coord Coord

data MyMaybe a = Nothing | Just a
-- If not we would need to do...
data MyMaybeInt = NothingInt | JustInt Int
data MyMaybeString = NothingString | JustString String
-- ... which is a bit exhausting 

--Prelude> :t id
--id :: a -> a

  
--Prelude> :t (+)
--(+) :: Num a => a -> a -> a


--Prelude> :t ["Patata"]
--["Patata"] :: [[Char]]


--"patata"::[Char] -- Concrete (not polymorphic)
--(+) :: Num a => a -> a -> a -- Constrained polymorphic
--id:: a -> a -- Parametrically polymorphic


--a -> a 
--Num a => a -> a -> a 
--Int -> Int -> Int


myfunction:: Int -> Int -> Int
myfunction x y = x + y


myfunction2:: a -> a -> a
myfunction2 x y = x 


myfunction3:: Num a => a -> a -> a
myfunction3 x y = x + y

