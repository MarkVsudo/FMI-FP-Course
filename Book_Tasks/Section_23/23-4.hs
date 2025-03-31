-- Да се реализира функция sublist l1 l2, която проверява дали всички
-- елементи на l1 са елементи и на l2.

sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _ = True  
sublist _ [] = False 
sublist (x:xs) l2
  | x `elem` l2 = sublist xs l2 
  | otherwise = False            


