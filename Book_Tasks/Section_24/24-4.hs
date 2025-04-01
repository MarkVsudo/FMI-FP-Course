-- Едно положително цяло число е съвършено, ако е равно на сумата от
-- своите делители (без самото число). Например, 6 е съвършено, защото
-- 6 = 1+2+3; числото 1 не е съвършено. Да се дефинира функция, коя-
-- то създава списък с всички съвършени числа, ненадминаващи дадено
-- положително цяло число в параметър n.

-- isPerfect :: Int -> Bool

getDivisors :: Int -> [Int]
getDivisors n = [x | x <- [1..n `div` 2], n `mod` x == 0]

isPerfect :: Int -> Bool
isPerfect n = sum (getDivisors n) == n

allPerfectNums :: Int -> [Int]
allPerfectNums n = [x | x <- [1..n], isPerfect x]


