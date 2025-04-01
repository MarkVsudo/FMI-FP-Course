-- Едно положително цяло число е съвършено, ако е равно на сумата от
-- своите делители (без самото число). Например, 6 е съвършено, защото
-- 6 = 1+2+3; числото 1 не е съвършено. Да се дефинира функция, която
-- проверява дали дадено положително цяло число е съвършено.

sumDivisors :: Int -> Int
sumDivisors n = helper (n - 1)
  where
    helper 0 = 0
    helper d = if n `mod` d == 0
               then d + helper (d - 1)
               else helper (d - 1)

isPerfect :: Int -> Bool
isPerfect n = n == sumDivisors n

isPerfect' :: Int -> Bool
isPerfect' n = n == sum [d | d <- [1..n-1], n `mod` d == 0]