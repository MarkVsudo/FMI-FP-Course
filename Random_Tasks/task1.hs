-- Дефинирайте функция sayReverse, която по едноцифрено число връща неговото име, но изписано наобратно.
-- Пример:
-- sayReverse 3 -> "eerht"

import Data.List

numStrs = ["zero"
           ,"one"
           ,"two"
           ,"three"
           ,"four"
           ,"five"
           ,"six"
           ,"seven"
           ,"eight"
           ,"nine"]

sayReverse :: Int -> String
sayReverse n = if n < 0 || n > 9 
               then "Invalid output"
               else  reverse (numStrs !! n)           