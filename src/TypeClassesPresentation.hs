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

data Pet a = Dog a



instance Eq a => Eq (Pet a) where
  (==) (Dog a) (Dog a') = a == a'

data NoEq = NoEqA | NoEqB

--instance Ord a => Eq (Pet a) where
-- (==) (Dog v) (Dog v') = compare v v' == EQ


-- You don't have to implement all of the methods by hand,
--

data Mood = Blah | Bleh

instance Show Mood where
  show Blah = "blah"
  show Bleh = "bleh"

