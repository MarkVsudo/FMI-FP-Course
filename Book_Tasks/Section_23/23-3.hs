-- Да се реализира функция index x l, която намира поредния номер на
-- първото срещане на елемента x в l. Например index 7 [1,2,7,3,2] -> 3.

index :: Int -> [Int] -> Int
index x l = indexHelper x l 1  

indexHelper :: Int -> [Int] -> Int -> Int
indexHelper _ [] _ = -1  
indexHelper x (l:ls) i
  | x == l    = i  
  | otherwise = indexHelper x ls (i + 1)  

