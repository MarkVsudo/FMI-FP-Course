-- 26.2
-- В един университет има студенти и преподаватели. Всеки студент има
-- факултетен номер, име и списък от курсове, които посещава. Всеки 
-- преподавател има име, списък от курсове, които преподава и номер на 
-- кабинет. Да се дефинира алгебричен тип Member със съответни конструктори
-- за студент и преподавател.
-- а) По списък [Member] да се намери броят на студентите.
-- б) Да се дефинира функция attendees :: String -> [Member] -> [Member],
-- която по име на предмент и списък от членове на унивеситета на-
-- мира броя на студентите, които слушат предмета.
-- в) Да се дефинира функция classmembers :: String -> [Member] ->
-- [Member], която по име на предмент и списък от членове на унивеситета намира списък 
-- само на тези членове, които преподат или слушат дадения предмет.
-- г) Да се дефинира функция namestitles :: [Member] -> [String],
-- която връща имената на всички членове на университета, като пред
-- името на всеки преподавател автоматично добавя титлата “проф.”.
-- д) (*) Да се дефинира функция bussiest :: [Member] -> Int, която
-- намира кабинета с най-много преподаватели.



-- Define the algebraic data type Member with two constructors: Student and Teacher
data Member = Student String String [String]   -- Student: Name, Faculty number, List of courses
            | Teacher String [String] Int      -- Teacher: Name, List of courses, Office number
            deriving (Show)

-- a) Count the number of students
countStudents :: [Member] -> Int
countStudents members = length [x | x <- members, isStudent x]
  where
    isStudent (Student _ _ _) = True
    isStudent _ = False

-- b) Find students attending a specific course
attendees :: String -> [Member] -> [Member]
attendees course members = [x | x <- members, isStudent x, course `elem` courses x]
  where
    isStudent (Student _ _ _) = True
    isStudent _ = False
    courses (Student _ _ cs) = cs
    courses _ = []

-- c) Find members who teach or attend a specific course
classmembers :: String -> [Member] -> [Member]
classmembers course members = [x | x <- members, course `elem` courses x]
  where
    courses (Student _ _ cs) = cs
    courses (Teacher _ cs _) = cs

-- d) Add titles for teachers
namestitles :: [Member] -> [String]
namestitles members = [titleMember x | x <- members]
  where
    titleMember (Teacher name _ _) = "Prof. " ++ name
    titleMember (Student name _ _) = name

-- e) Find the office with the most teachers
bussiest :: [Member] -> Int
bussiest members = fst $ maximumBy (comparing snd) officeCounts
  where
    officeCounts = [(office, length (filter (\x -> office == officeOf x) teachers)) 
                    | office <- officeNumbers, let teachers = [x | x <- members, isTeacher x]]
    officeNumbers = nub [officeOf x | x <- members, isTeacher x]
    officeOf (Teacher _ _ office) = office
    officeOf _ = 0
    isTeacher (Teacher _ _ _) = True
    isTeacher _ = False

