-- Да се дефинира функция, която в даден низ замества всички малки
-- латински букви със съответните им големи латински букви.
import Data.Char


toUpperCase :: String -> String
toUpperCase str = map (\c -> toUpper c) str

toUpperCase' :: String -> String
toUpperCase' = map toUpper