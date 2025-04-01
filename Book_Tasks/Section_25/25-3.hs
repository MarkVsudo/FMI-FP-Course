-- Да се дефинира функция countEvenOddl, която за списъка от цели числа
-- l връща наредена двойка от броя на четните и броя на нечетните числа в l.

countEvenOddl :: [Int] -> (Int, Int)
countEvenOddl [] = (0, 0)  
countEvenOddl xs = (length evenList, length oddList)
  where evenList = [e | e <- xs, even e]
        oddList  = [o | o <- xs, odd o]
