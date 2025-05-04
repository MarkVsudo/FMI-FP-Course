import Data.List
import Data.Function

main = do
  print $ take 10 allpairs1
  print $ take 10 allpairs2
  print $ take 10 allpairs3
  print $ closestToAverage store1
  print $ closestToAverage' store1
  print $ cheaperAlternative store2
  print $ cheaperAlternative' store2
  print $ cheaperAlternative'' store2
  print $ hardestSubject students
  print $ cP [ Present, Late, Present, Absent
             , Present, Present, Present, Absent ]
  print $ cP [ Present, Late, Present, Late, Present
             , Late, Present, Absent, Late, Present]
  print $ cP [ Present, Late, Present, Late, Late, Late
             , Present, Present, Absent, Present]
  print $ canPass' (1, 2) [ Present, Late, Present, Absent
                          , Present, Present, Present, Absent ]
  print $ canPass' (1, 2) [ Present, Late, Present, Late, Present
                          , Late, Present, Absent, Late, Present]
  print $ canPass' (1, 2) [ Present, Late, Present, Late, Late, Late
                          , Present, Present, Absent, Present]
  print $ canPass'' (1, 2) [ Present, Late, Present, Absent
                           , Present, Present, Present, Absent ]
  print $ canPass'' (1, 2) [ Present, Late, Present, Late, Present
                           , Late, Present, Absent, Late, Present]
  print $ canPass'' (1, 2) [ Present, Late, Present, Late, Late, Late
                           , Present, Present, Absent, Present]
  print $ getOddCompositionValue [ (\ x -> x + 1)
                                 , (\ x -> x * 2)
                                 , (\ x -> x - 1)
                                 , (\ x -> x `div` 2)]
                                 2
  print $ getOddCompositionValue [ (\ x -> x + 1)
                                 , (\ x -> x * 2)
                                 , (\ x -> x * 10)
                                 , (\ x -> x `div` 2)]
                                 2
  print $ getOddCompositionValue' [ (\ x -> x + 1)
                                  , (\ x -> x * 2)
                                  , (\ x -> x - 1)
                                  , (\ x -> x `div` 2)]
                                  2
  print $ getOddCompositionValue' [ (\ x -> x + 1)
                                  , (\ x -> x * 2)
                                  , (\ x -> x * 10)
                                  , (\ x -> x `div` 2)]
                                  2
  where
    cP = canPass (1, 2)

allpairs1 = [(x, y) | x <- [1..], y <- [1..]]
allpairs2 = [(x, y) | x <- [1..], x <= 10, y <- [1..]]
allpairs3 = [(x, y) | x <- [1..], y <- [1..], x <= 10]

type Product = (String, Double)
type StoreAvailability = [Product]

store1 = [("bread",1),("milk",2.5),("lamb",10),("cheese",5),("butter",2.3)]

closestToAverage :: StoreAvailability -> String
closestToAverage store =
  snd (minimum [(abs (price - averagePrice), name) | (name, price) <- store])
  where
    average xs = sum xs / fromIntegral (length xs)
    averagePrice = average [price | (_, price) <- store]

closestToAverage' :: StoreAvailability -> String
closestToAverage' store =
  fst (minimumBy (compare `on` (\ (_, price) -> abs (price - averagePrice)))
                 store)
  where
    average xs = sum xs / fromIntegral (length xs)
    averagePrice = average [price | (_, price) <- store]

store2 = [("bread",1),("cheese",2.5),("bread",1),("cheese",5),("butter",2.3)]

cheaperAlternative :: StoreAvailability -> Int
cheaperAlternative store =
  length [p | p <- products, length (nub (getPrices p)) > 1]
  where
    products = nub [name | (name, _) <- store]
    getPrices p = [price | (name, price) <- store, name == p]

cheaperAlternative' :: StoreAvailability -> Int
cheaperAlternative' store =
  length (filter (\ ps -> length ps > 1)
         (map nub
              (map (map snd)
                   (groupBy ((==) `on` fst)
                            (sort store)))))

-- (==) `on` fst  ===  (\ p1 p2 -> fst p1 == fst p2)

cheaperAlternative'' :: StoreAvailability -> Int
cheaperAlternative'' =
  length
    . (filter ((> 1) . length))
    . (map nub)
    . (map (map snd))
    . (groupBy ((==) `on` fst))
    . sort

type Student = String -- име на ученик
type Subject = String -- име на предмет
type Note = Double -- оценка
-- Запис за ученик, съдържащ име на ученик, учебен предмет и оценката на
-- ученика по дадения предмет.
type Record = (Student, Subject, Note)

students :: [Record]
students = [ ("Ivan", "Matematika", 5)
           , ("Petar", "Matematika", 4)
           , ("Ivan", "BEL", 3)
           , ("Petar", "BEL", 4)
           , ("Ivan", "Fizika", 6)
           , ("Petar", "Fizika", 6) ]

hardestSubject :: [Record] -> Subject
hardestSubject students =
  snd (minimum [(average (getNotes s), s) | s <- subjects])
  where
    subjects = nub [subject | (_, subject, _) <- students]
    getNotes s = [note | (_, subject, note) <- students,
                         subject == s]
    average xs = sum xs / (fromIntegral (length xs))

type Misses = Int
type Lates = Int
type Criterion = (Misses, Lates)
data Attendance = Absent | Late | Present deriving (Eq, Show)
type StudentRecord = [Attendance]

canPass :: Criterion -> (StudentRecord -> Bool)
canPass (n, k) =
  \ records -> cond1 records && cond2 records
  where
    cond1 records =
      length (filter (== Absent) records) <= n
    cond2 records =
      maximum (map length
              (filter ((== Late) . head)
                      (group records))) <= k

canPass' :: Criterion -> (StudentRecord -> Bool)
canPass' (n, k) = check
  where
    check records = cond1 records && cond2 records
    cond1 records =
      length (filter (== Absent) records) <= n
    cond2 records =
      maximum (map length
              (filter ((== Late) . head)
                      (group records))) <= k

canPass'' :: Criterion -> (StudentRecord -> Bool)
canPass'' (n, k) records =
  cond1 && cond2
  where
    cond1 = length (filter (== Absent) records) <= n
    cond2 = maximum (map length
                    (filter ((== Late) . head)
                            (group records))) <= k

getOddCompositionValue :: [Int -> Int] -> (Int -> Int)
getOddCompositionValue fs =
  \ x -> (foldr1 (.) oddFunctions) x
  where
    oddFunctions = [f | (f, i) <- zip fs [1..], odd i]

getOddCompositionValue' :: [Int -> Int] -> (Int -> Int)
getOddCompositionValue' fs =
  foldr1 (.) [f | (f, i) <- zip fs [1..], odd i]