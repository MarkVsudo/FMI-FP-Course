import Data.List
import Data.Function


main :: IO()
main = do
  print 6
  print d1


-- Задачи за самоподготовка. 26.1
data DayOfWeek =  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Read, Eq, Ord)
--  deriving Show

instance Show DayOfWeek where
  show Mon = "Monday"
  show Tue = "Tuesday"
  show Wed = "Wednesday"
  show Thu = "Thursday"
  show Fri = "Friday"
  show Sat = "Saturday"
  show Sun = "Sunday"

dayToInt :: DayOfWeek -> Int
dayToInt Mon = 1
dayToInt Tue = 2
dayToInt Wed = 3
dayToInt Thu = 4
dayToInt Fri = 5
dayToInt Sat = 6
dayToInt Sun = 7

intToDay :: Int -> DayOfWeek
intToDay 1 = Mon
intToDay 2 = Tue
intToDay 3 = Wed
intToDay 4 = Thu
intToDay 5 = Fri
intToDay 6 = Sat
intToDay 7 = Sun

prevDay :: DayOfWeek -> DayOfWeek
prevDay d = intToDay $ mod ((dayToInt d - 1) + 6) 7 + 1

nextDay :: DayOfWeek -> DayOfWeek
nextDay d = intToDay $ mod ((dayToInt d - 1) + 1) 7 + 1

daysInterval :: DayOfWeek -> DayOfWeek -> [DayOfWeek]
daysInterval begin end =
  res ++ [nextDay (last res)]
  where
    daysOfWeek = [intToDay d | d <- [1..7]]
    res = takeWhile (/= end) (dropWhile (/= begin) (daysOfWeek ++ daysOfWeek))

daysInterval' :: DayOfWeek -> DayOfWeek -> [DayOfWeek]
daysInterval' begin end =
  take (if count < 0 then count + 7 else count)
       (dropWhile (/= begin) (daysOfWeek ++ daysOfWeek))
  where
    daysOfWeek = [intToDay d | d <- [1..7]]
    count = dayToInt end - dayToInt begin + 1

d1 :: DayOfWeek
d1 = Fri

-- Задачи за самоподготовка. 26.2
type FacNo = Int
type RoomNo = Int
type Name = String
type Course = String
data Member = Student FacNo Name [Course]
            | Teacher Name [Course] RoomNo
  deriving (Show, Eq)

members :: [Member]
members = [Student 1 "Ivan" ["FP", "OOP"],
           Student 2 "Petar" ["FP"],
           Teacher "Ivanov" ["OOP", "UP"] 123,
           Teacher "Georgiev" ["DAA"] 123,
           Teacher "Angelov" ["SDP", "FP"] 124]

countStudents :: [Member] -> Int
countStudents ms =
  length [m | m@(Student _ _ _) <- ms]

attendees :: Course -> [Member] -> [Member]
attendees course ms =
  [m | m@(Student _ _ cs) <- ms, elem course cs]

classmembers :: Course -> [Member] -> [Member]
classmembers course ms =
  [m | m <- ms, elem course (getCourses m)]
  where
    getCourses (Student _ _ cs) = cs
    getCourses (Teacher _ cs _) = cs

classmembers' :: Course -> [Member] -> [Member]
classmembers' course ms =
  filter (\ m -> elem course (getCourses m)) ms
  where
    getCourses (Student _ _ cs) = cs
    getCourses (Teacher _ cs _) = cs

classmembers'' :: Course -> [Member] -> [Member]
classmembers'' course =
  filter ((elem course) . getCourses)
  where
    getCourses (Student _ _ cs) = cs
    getCourses (Teacher _ cs _) = cs

namestitles :: [Member] -> [Name]
namestitles = map getName
  where
    getName (Student _ name _) = name
    getName (Teacher name _ _) = "Prof. " ++ name

bussiest :: [Member] -> RoomNo
bussiest ms = maximumBy (compare `on` count) rooms
  where
    allRooms = [room | (Teacher _ _ room) <- ms]
    rooms = nub allRooms
    count room = length (filter (== room) allRooms)

bussiest' :: [Member] -> RoomNo
bussiest' ms =
  foldl1 (\ a b -> if count a > count b then a else b)
         rooms
  where
    allRooms = [room | (Teacher _ _ room) <- ms]
    rooms = nub allRooms
    count room = length (filter (== room) allRooms)

-- data Maybe a = Nothing | Just a

m1 :: Maybe Int
m1 = Just 123

-- find (== 10) [1..9]      -> Nothing
-- find (== 10) [10,1,10,2] -> Just 10
-- find odd [10,1,10,2]     -> Just 1

type Scale = Maybe (Double, Double)
s1 :: Scale
s1 = Just (0, 0)

left :: Double -> Scale -> Scale
left x (Just (l, r)) = validate $ Just (l + x, r)
left _ Nothing = Nothing

right :: Double -> Scale -> Scale
right x (Just (l, r)) = validate $ Just (l, r + x)
right _ Nothing = Nothing

validate :: Scale -> Scale
validate Nothing = Nothing
validate s@(Just (l, r)) =
  if abs (l - r) > 50 || l > 100 || r > 100
    then Nothing
    else s