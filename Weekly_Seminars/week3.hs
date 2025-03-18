import Data.Char
import Data.List

main :: IO()
main = do
  print [1,2,3]
  print [1..10]
  print (take 3 [1..10])
  print (drop 3 [1..10])
  print (take 10 [0..])
  print (1, "abc", 1.25)
  print (zip [1,2,3,4,5] ["a", "bb", "ccc"])
  print (zip ["a", "bb", "ccc"] [0..])
  print [cs | (cs, i) <- zs, even i]
  print [fst z | z <- zs, even (snd z)]
  print (snd (1,2))
  print (snd3 (1,2,3))
  print (zipWith (+) [1, 2, 3] [4, 5, 6])
  print (take 15 fib)
  print (sublist [1,2,4,0] [1,2,3,4])
  print (sublist' [1,2,4,0] [1,2,3,4])
  print (sublist'' [1,2,4,0] [1,2,3,4])
  where zs = zip ["a", "bb", "ccc"] [0..]


xs :: [Int]
xs = [1,2,3]

v1 :: (Int, String, Double)
v1 = (1, "abc", 1.25)

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- Fibonacci sequence
-- 0, 1, 1, 2, 3, 5, 8, 13, ...
-- 0, 1, 1, 2, 3, 5, ...
--    0, 1, 1, 2, 3, 5, ...
fib :: [Integer]
fib = 0 : 1 : zipWith (+) (tail fib) fib

-- 23.4. Да се реализира функция sublist l1 l2,
--       която проверява дали всички
--       елементи на l1 са елементи и на l2.
sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _ = True
sublist (x:xs) ys =
  elem x ys && sublist xs ys

sublist' :: (Eq a, Ord a) => [a] -> [a] -> Bool
sublist' xs ys = helper (sort xs) (sort ys)
  where
    helper []        _  = True
    helper _         [] = False
    helper as@(x:xs) (y:ys)
      | x == y    = helper xs ys
      | x > y     = helper as ys
      | otherwise = False

sublist'' :: Eq a => [a] -> [a] -> Bool
sublist'' xs ys = null [x | x <- xs, not (elem x ys)]
-- [1,2,4] [1,2,3,4]

-- 23.6. Да се реализира функция duplicates l,
--       която проверява дали в списъка l има 
--       повтарящи се елементи.

duplicates :: Eq a => [a] -> Bool
duplicates xs = not (null [x | x <- xs, elem x (delete x xs)])

duplicates' :: Eq a => [a] -> Bool
duplicates' xs = xs /= nub xs

-- 24.2. Да се дефинира функция, която по дадено естествено число n
--       връща списък с цифрите му, четени отдясно на ляво.
toNums :: Int -> [Int]
toNums n =
  if n < 10 then [n]
  else mod n 10 : toNums (div n 10)

toNums' :: Int -> [Int]
toNums' n =
  reverse [ord c - ord '0' | c <- show n]

toNums'' :: Int -> [Int]
toNums'' n =
  reverse [digitToInt c | c <- show n]

toNums''' :: Int -> [Int]
toNums''' n = reverse (map digitToInt (show n))

toNums'''' :: Int -> [Int]
toNums'''' = reverse . (map digitToInt) . show

-- Композиция
-- (f . g) x = f (g x)

-- 25.7. Да се дефинира функция, която по даден списък от цели числа l
--       връща списък от всички двойки (a, b) от l, за които a и b са 
--       съседни елементи в l и a < b.
-- getSpecialPairs [1,2,5,3,2,5,6] -> [(1,2), (2,5), (2,5), (5,6)]

getSpecialPairs :: [Int] -> [(Int, Int)]
getSpecialPairs ns = [p | p@(a, b) <- zip ns (tail ns), a < b]

getSpecialPairs' :: [Int] -> [(Int, Int)]
getSpecialPairs' []  = []
getSpecialPairs' [_] = []
getSpecialPairs' (n1:n2:ns) =
  if n1 < n2
  then (n1, n2) : getSpecialPairs' (n2:ns)
  else getSpecialPairs' (n2:ns)