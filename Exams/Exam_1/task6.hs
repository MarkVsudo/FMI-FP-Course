-- Задача 6. Да се дефинира функция, която приема списък [a] и заменя всички непрекъснати последователности от еднакви елементи x с единичен елемент x.

-- Пример: за списъка [0,0,1,2,2,2,3,4], резултатът е [0,1,2,3,4]

import Data.List

main :: IO()
main = do
  print $ task6 [0,0,1,2,2,2,3,4]
  print $ task6' [0,0,1,2,2,2,3,4]

task6 :: Eq a => [a] -> [a]
task6 xs = [x | (x:_) <- group xs]

task6' :: Eq a => [a] -> [a]
task6' =
  foldr (\ x rs -> if null rs then [x]
                   else if x == head rs then rs
                   else x : rs)
        []