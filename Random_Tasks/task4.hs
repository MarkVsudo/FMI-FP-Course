-- Дефинирайте функция swapCase, която заменя всички малки букви със съответните им главни, а главните — с малки.
-- Пример:
-- swapCase "HeLLo" -> "hEllO"

import Data.Char 

swapCase :: String -> String
swapCase [] = []  
swapCase (s:str)
  | isUpper s = toLower s : swapCase str  
  | isLower s = toUpper s : swapCase str  
  | otherwise = s : swapCase str          

