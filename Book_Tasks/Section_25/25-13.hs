-- Да се дефинира функция remove x l, която премахва: 
-- (а) първото срещане на елемента x от списъка l и 
-- (б) всички срещания на елемента x от списъка l.
import Data.List

removeA :: Int -> [Int] -> [Int]
removeA x l = delete x l 

removeB :: Int -> [Int] -> [Int]
removeB x l = filter (\el -> el /= x) l