-- Дефинирайте функция countPositiveNegative, която за списък от цели числа връща наредена двойка с броя на положителните и отрицателните числа.
-- Пример:
-- countPositiveNegative [-5, 2, 3, -1, -9, 7] -> (3, 3)

countPositiveNegative :: [Int] -> (Int, Int)
countPositiveNegative [] = (0, 0)
countPositiveNegative xs = (length posList, length negList)
      where posList = filter (\el -> el > 0) xs
            negList = filter (\el -> el < 0) xs