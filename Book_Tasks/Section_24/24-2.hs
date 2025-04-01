-- Да се дефинира функция, която по дадено естествено число n връща
-- списък с цифрите му, четени отдясно на ляво.

reverseNum :: Int -> [Int]

reverseNum n 
  | n < 10    = [n]
  | otherwise = (n `mod` 10) : reverseNum (n `div` 10)  
