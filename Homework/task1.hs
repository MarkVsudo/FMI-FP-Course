-- Задача 1. Да се дефинира функция encode :: Eq a => [a] -> [(Int, a)],
-- която получава списък xs и връща списък от двойки, в който първият елемент на
-- двойката е броят на последователните еднакви елементи от xs, а вторият елемент е
-- самият елемент.
-- Примери:
-- encode [1,1,1,2,2,3,4,4,4,4] → [(3, 1), (2, 2), (1, 3), (4, 4)]
-- encode [1,2,2,3,3,4,4] → [(1, 1), (2, 2), (2, 3), (2, 4)]
-- encode [1,2,1,2,2,1,1] → [(1, 1), (1, 2), (1, 1), (2, 2), (2, 1)]

import Data.List

encode :: Eq a => [a] -> [(Int, a)]

encode [] = []
-- Solution 1
encode xs =  (length (head(group xs)), head(head(group xs))) : encode (drop (length (head(group xs))) xs)

-- Solution 2
encode' xs = [(length pair, head pair) | pair <- group xs]
 
