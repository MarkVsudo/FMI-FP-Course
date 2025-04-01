-- Да се напише функция, която намира сумата на цифрите в десетичния
-- запис на дадено естествено число.

sumDigits :: Int -> Int
sumDigits n =
  if n < 10 then n
  else (n `mod` 10) + sumDigits (n `div` 10)