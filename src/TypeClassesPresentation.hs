module TypeClassesPresentation where

-- :info Bool
--Prelude> :info Bool
--data Bool = False | True
--instance Eq Bool
--instance Ord Bool
--instance Show Bool
--instance Read Bool
--instance Enum Bool
--instance Bounded Bool

areTheyEqual:: Eq a => a -> a -> String
areTheyEqual x y = 
  if (x == y) then "They are the same thing" 
  else "MEEEECK" 
  
data Pet = Cat | Dog String

whatPet:: Pet -> String
whatPet (Cat) = "it is a cat"
whatPet (Dog x) = "it is a dog"

instance Eq Pet where
  (==) Cat Cat = True
  (==) (Dog x) (Dog y) = x == y 
  (==) _ _ = False

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun
-- day of week and numerical day of month
data Date = Date DayOfWeek Int


instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'


--- multiple definitions for a single function
crazyfun :: [Int] -> Int
crazyfun [] = 0
crazyfun (x:_) = x

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
-----------

data NoEq = NoEqA | NoEqB

--instance Ord a => Eq (Pet a) where
-- (==) (Dog v) (Dog v') = compare v v' == EQ


-- You don't have to implement all of the methods by hand,
--

data Mood = Blah | Bleh

instance Show Mood where
  show Blah = "blah"
  show Bleh = "bleh"
  
data SpecialPet a = Snake a

instance Eq a => Eq (SpecialPet a) where
  (==) (Snake a) (Snake a') = a == a'

--[1 of 1] Compiling TypeClassesPresentation ( src/TypeClassesPresentation.hs, interpreted )
--
--src/TypeClassesPresentation.hs:77:31: error:
--    • No instance for (Eq a) arising from a use of ‘==’
--      Possible fix: add (Eq a) to the context of the instance declaration
--    • In the expression: a == a'
--      In an equation for ‘==’: (==) (Snake a) (Snake a') = a == a'
--      In the instance declaration for ‘Eq (SpecialPet a)’
--   |
--77 |   (==) (Snake a) (Snake a') = a == a'
--   |                               ^^^^^^^
