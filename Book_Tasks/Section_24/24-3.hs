-- (*) Да се дефинира функция, която по дадено естествено число n връ-
-- ща списък с цифрите му, четени отдясно на ляво, без повторения на елементите на списъка.

import Data.List

reverseNum :: Int -> [Int]
reverseNum n = nub $ digitsReverse n
  where
    digitsReverse 0 = []
    digitsReverse x = (x `mod` 10) : digitsReverse (x `div` 10)

