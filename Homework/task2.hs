-- Задача 2. Да се дефинира функция mergeEvenOdd :: [Int] -> [Int] -> [Int], която получава два списъка от цели числа as и bs и връща списък, чиито
-- елементи на четни позиции са елементите на as, а тези на нечетни позиции са
-- елементите на bs. Функцията да работи до изчерпване на по-късия от двата списъка.
-- Примери:
-- mergeEvenOdd [1,2,3] [4,5,6] → [1,4,2,5,3,6]
-- mergeEvenOdd [1,2] [6..] → [1,6,2,7]
-- mergeEvenOdd [1,2] [6,7,8] → [1,6,2,7]

mergeEvenOdd :: [Int] -> [Int] -> [Int]
mergeEvenOdd [_] [] = []
mergeEvenOdd [] [_] = []
mergeEvenOdd [] [] = []
mergeEvenOdd (a:as) (b:bs) = [a, b] ++ mergeEvenOdd as bs
