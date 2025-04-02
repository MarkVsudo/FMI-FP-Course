-- Дефинирайте функция splitAround x l, която разделя списъка на две части: първата съдържа елементите по-малки или равни на x, а втората — останалите.
-- Пример:
-- splitAround 5 [3, 7, 1, 5, 8, 2] -> ([3,1,5,2], [7,8])

splitAround :: Int -> [Int] -> ([Int], [Int])
splitAround _ [] = ([], [])
splitAround n xs = (filter (<=n) xs, filter (>n) xs)

