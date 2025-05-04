import Data.List 
import Data.Ord 

-- 26.1
-- Да се дефинира тип DayOfWeek, представящ ден от седмицата. 
-- За типа да се реализират възможност за:
-- • Въвеждане и извеждане
-- • Сравнение на дни с ==,<,>
-- • Намиране на следващ и предишен ден от седмицата
-- • Преобразуване от и до число
-- • Създаване на интервал от дни
-- Съответните възможности да се добавят по два начина:
-- • Чрез нарочни функции за целта
-- • Чрез наследяване на базови класове

main :: IO ()
main = do 
  -- 26.1
  print Monday
  let monday = read "Monday" :: DayOfWeek
  print monday
  print (Monday == Monday)  
  print (Monday > Friday)  
  print (Monday < Friday)  
  print (findNextDay Monday)
  print (findNextDay Sunday)
  print (findPreviousDay Monday)
  print (findPreviousDay Tuesday)
  print (createInterval Monday Friday)
  print (convertToNumber Monday)
  print (convertFromNumber 0)
  -- 26.2
  let members = [Student "Ivan" "123" ["Math", "Programming"], 
                 Student "Maria" "124" ["Math", "Biology"], 
                 Teacher "Dr. Stefan" ["Math", "Programming"] 101, 
                 Teacher "Prof. Elena" ["Biology"] 102]
  print $ countStudents members  -- 2
  print $ attendees "Math" members  -- [Student "Ivan" "123" ["Math", "Programming"], Student "Maria" "124" ["Math", "Biology"]]
  print $ classmembers "Math" members  -- [Student "Ivan" "123" ["Math", "Programming"], Student "Maria" "124" ["Math", "Biology"], Teacher "Dr. Stefan" ["Math", "Programming"] 101]
  print $ namestitles members  -- ["Ivan", "Maria", "Prof. Dr. Stefan", "Prof. Prof. Elena"]
  print $ bussiest members  -- 101
  

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
                 deriving (Show, Read, Eq, Ord, Enum)

findNextDay :: DayOfWeek -> DayOfWeek
findNextDay day
  | day == Sunday = Monday
  | otherwise = succ day

findPreviousDay :: DayOfWeek -> DayOfWeek
findPreviousDay day
  | day == Monday = Sunday
  | otherwise = pred day

createInterval :: DayOfWeek -> DayOfWeek -> [DayOfWeek]
createInterval start end
    | fromEnum start <= fromEnum end = [start .. end] 
    | otherwise = reverse [end .. start]

convertToNumber :: DayOfWeek -> Int
convertToNumber day = fromEnum day

convertFromNumber :: Int -> DayOfWeek
convertFromNumber num = toEnum num :: DayOfWeek






