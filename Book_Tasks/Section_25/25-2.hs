-- Функция, която намира дължината на най-дългия общ префикс на два списъка
longestCommonPrefixLength :: Eq a => [a] -> [a] -> Int
longestCommonPrefixLength xs ys = length (takeWhile id (zipWith (==) xs ys))

main :: IO ()
main = do
    let list1 = ["a", "b", "c", "d"]
    let list2 = ["a", "b", "x", "y"]
    print (longestCommonPrefixLength list1 list2)  
