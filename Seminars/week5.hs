import Data.List


main :: IO()
main = do
  print (f1 1 2 3)
  print (f1 2 2 3)
  print ((\ x -> x * 2) 10)
  print (f1' 1 2 3)
  print (map (\ x -> x * 5) [1..5])
  print (map (* 5) [1..5])
  print (map (\ x -> x * 5 + 2) [1..5])
  print (map ((+ 2) . (* 5)) [1..5])
  print ((f1 1 2) 3)
  print (((f1 1) 2) 3)


f1' = \ a b c -> a + (b * c)
f1'' = helper
  where helper a b c = a + (b * c)

-- Int -> Int -> Int -> Int
-- Int -> (Int -> Int) -> Int

f1 :: Int -> (Int -> (Int -> Int))
f1 a = \ b c -> a + (b * c)
-- f1 a b c = a + (b * c)

--- (f . g) x = f (g x)

-- f1 a b c = a + (b * c)

f2 x = x * 5 + 2
f2' = (+ 2) . (* 5)

-- Домашно 1. Задача 1
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\ gs@(g:_) -> (length gs, g)) . group

-- Домашно 1. Задача 2
mergeEvenOdd :: [Int] -> [Int] -> [Int]
mergeEvenOdd (a:as) (b:bs) = a : b : mergeEvenOdd as bs
mergeEvenOdd _      _      = []


-- Решете си за самостоятелно домашно:
  -- Задачи за самоподготовка. 26.1
  -- Задачи за самоподготовка. 26.2