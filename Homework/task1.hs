-- Задача 1. Да се дефинира функция encode :: Eq a => [a] -> [(Int, a)],
-- която получава списък xs и връща списък от двойки, в който първият елемент на
-- двойката е броят на последователните еднакви елементи от xs, а вторият елемент е
-- самият елемент.
-- Примери:
-- encode [1,1,1,2,2,3,4,4,4,4] → [(3, 1), (2, 2), (1, 3), (4, 4)]
-- encode [1,2,2,3,3,4,4] → [(1, 1), (2, 2), (2, 3), (2, 4)]
-- encode [1,2,1,2,2,1,1] → [(1, 1), (1, 2), (1, 1), (2, 2), (2, 1)]

import Data.List

-- Solution 1
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs =  (length (head(group xs)), head(head(group xs))) : encode (drop (length (head(group xs))) xs)

-- Solution 2
encode' :: Eq a => [a] -> [(Int, a)]
encode' [] = []
encode' xs = [(length g, head g) | g <- group xs]
 
-- Solution 3
encode'' :: Eq a => [a] -> [(Int, a)]
encode'' [] = []
encode'' (x:xs) = helper 1 x xs
  where
    helper count elem [] = [(count, elem)]
    helper count elem (y:ys)
      | elem == y  = helper (count + 1) elem ys  -- Increment count if same element
      | otherwise  = (count, elem) : helper 1 y ys  -- Start a new group


