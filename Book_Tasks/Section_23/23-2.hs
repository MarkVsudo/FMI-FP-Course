-- Да се реализира функция count x l, която преброява колко пъти се
-- среща елемента x в l.

count :: Int -> [Int] -> Int
count _ [] = 0  
count x (l:ls) = (if x == l then 1 else 0) + count x ls  
