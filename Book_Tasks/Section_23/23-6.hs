-- Да се реализира функция duplicates l, която проверява дали в списъка
-- l има повтарящи се елементи.

duplicates :: Eq a => [a] -> Bool

duplicates [] = False
duplicates (x:xs) 
  | x `elem` xs = True
  | otherwise = duplicates xs