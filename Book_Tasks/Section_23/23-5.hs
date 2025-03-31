-- Да се реализира функция common l1 l2, която преброява колко от еле-
-- ментите на l1 са елементи и на l2.

common :: Eq a => [a] -> [a] -> Int
common [] _ = 0
common _ [] = 0
common (x:xs) l2
  | x `elem` l2 = 1 + common xs l2  
  | otherwise   = common xs l2       