main :: IO()
main = do
  print $ fib 5
  print $ fib' 5
  print $ fib'' 5
  print $ fibIter 50


fib :: Integer -> Integer 
fib n = if n == 0
        then 0
        else if n == 1
             then 1
             else fib (n - 2) + fib (n - 1)

fib' :: Integer -> Integer 
fib' n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fib (n - 2) + fib (n - 1)


fib'' :: Integer -> Integer 
fib'' 0 = 0
fib'' 1 = 1
fib'' n = fib (n - 2) + fib (n - 1)


fibIter :: Int -> Integer
fibIter n = helper 0 1 n
  where
    helper cur next n =
      if n == 0
      then cur
      else helper next (cur + next) (n - 1)

fibIter' :: Int -> Integer
fibIter' n = helper 0 1 n
  where
    helper cur _    0 = cur
    helper cur next n = helper next (cur + next) (n - 1)

fibIter'' :: Int -> Integer
fibIter'' n = helper 0 1 0
  where
    helper cur next i =
      if i == n
      then cur
      else helper next (cur + next) (i + 1)

add5 :: Int -> Int
add5 x = x + 5

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs =
  if null xs
  then []
  else f (head xs) : map f (tail xs)

f2 :: Num a => a -> a
f2 x = x + 5

reverse' :: [a] -> [a]
reverse' xs =  helper xs []
  where
    helper []     res = res
    helper (x:xs) res = helper xs (x:res)

-- reverse [1,2,3] -> helper [1,2,3] [] -> helper [2,3] (1:[])
--   -> helper [3] (2:1:[]) -> helper [] (3:2:1:[]) -> (3:2:1:[])
--   -> 3:2:[1] -> 3:[2,1] -> [3,2,1]

-- 22.4
countDigits :: Int -> Int
countDigits n =
  if n < 10 then 1
  else 1 + countDigits (n `div` 10)

-- 22.5
sumDigits :: Int -> Int
sumDigits n =
  if n < 10 then n
  else n `mod` 10 + sumDigits (n `div` 10)

-- 22.11
sumDivisors :: Int -> Int
sumDivisors n = helper (n - 1)
  where
    helper 0 = 0
    helper d = if n `mod` d == 0
               then d + helper (d - 1)
               else helper (d - 1)
               
-- 6, 8128
isPerfect :: Int -> Bool
isPerfect n = n == sumDivisors n

isPerfect' :: Int -> Bool
isPerfect' n = n == sum [d | d <- [1..n-1], n `mod` d == 0]


-- 24.1.a
firstEvenNumbers :: Int -> [Int]
firstEvenNumbers n = helper 1 [0]
  where
    helper i res =
      if i == n then reverse res
      else helper (i + 1) ((head res + 2):res)

firstEvenNumbers' :: Int -> [Int]
firstEvenNumbers' n = [2 * i | i <- [0..n-1]]

firstEvenNumbers'' :: Int -> [Int]
firstEvenNumbers'' n = take n [0,2..]