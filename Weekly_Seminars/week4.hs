import Data.List

main :: IO()
main = do
  print $ primesInRange 1 100
  print $ groupsOf [1..10] 3
  print $ flattenl [[1,2],[3,4],[5]]
  print $ foldl (++) [] [[1,2],[3,4],[5]]
  print (1 + 2)
  print ((+) 1 2)
  print (mod 10 3)
  print (10 `mod` 3)
  print ([] :: [Int])

-- Задача. Да се напише функция primesInRange a b,
--   която връща простите числа в интервала [a, b].
isPrime :: Int -> Bool
isPrime n = [d | d <- [1..n], n `mod` d == 0] == [1, n]

primesInRange :: Int -> Int -> [Int]
primesInRange a b = [n | n <- [a..b], isPrime n]

-- 25.8. Да се дефинира функция groupsOf xs n, която разделя списъка xs на
--   групи от по n елемента.
-- groupsOf [1, 2, 3, 4, 5, 6, 7, 8] 3 → [[1, 2, 3], [4, 5, 6], [7, 8]]
groupsOf :: [a] -> Int -> [[a]]
groupsOf [] _ = []
groupsOf xs n = take n xs : groupsOf (drop n xs) n

-- 25.9. Да се дефинира функция flattenl :: [[a]] -> [a], която получава списък от
--   списъци и връща списък, който съдържа всички елементи на входните списъци.
flattenl :: [[a]] -> [a]
flattenl [] = []
flattenl (xs:xss) = xs ++ flattenl xss

(+++) :: Int -> Int -> Int
(+++) a b = a * (2 + b)

(++++) :: Int -> Int -> Int
a ++++ b = a * (2 + b)

-- 25.11. Да се дефинира функция pack :: Eq a => [a] -> [[a]], която получава
--   списък и връща списък от списъци, като всеки от тях съдържа всички
--   последователни еднакви елементи на входния списък.
-- pack [1, 1, 1, 2, 2, 3, 4, 4, 4, 4] → [[1, 1, 1], [2, 2], [3], [4, 4, 4, 4]]
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = helper revXs [[x]]
  where
    (x:revXs) = reverse xs
    helper [] res = res
    helper (x:xs) res@(zs:zss) =
      if x == head zs then helper xs ((x:zs):zss)
      else helper xs ([x]:res)

pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (x : (takeWhile (== x) xs)) : pack' (dropWhile (== x) xs)

-- 25.15. За даден списък xs, да се намерят елементите на списъка, чиято
--   стойност е по-голяма от сумата на предхождащите ги елементи.
-- findHigher [1, 2, 5, 9, 16] → [1, 2, 5, 9]