-- Да се напише функция, която намира броя на цифрите в десетичния
-- запис на дадено естествено число.

main :: IO()
main = do 
  print (findDigitsCount 1134)
  print (findDigitsCount 1)

findDigitsCount x = if x < 10 then 1 else 1 + findDigitsCount (x `div` 10)