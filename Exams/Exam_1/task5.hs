-- Задача 5. Дадени са списък от двойки xs :: [(a,a)] и списък choice :: [Bool]. За всеки елемент от x :: (a,a) от xs, съответния му елемент (като поредност) ch :: Bool от choice определя дали се интересуваме от първия или втория елемент на x. Ако ch е истина, вземаме първия компонент на x, а в противен случай - втория. Да се напише функция, която при подадени списъци xs и choice построява списък [a] по горното правило.

-- Пример: За списъците xs = [(0,1),(0,1),(0,1)] и choice = [True, False, True], резултатът ще е [0,1,0].

main :: IO()
main = do
  print $ task5 [(0,1),(0,1),(0,1)] [True, False, True]

task5 :: [(a, a)] -> [Bool] -> [a]
task5 xs choice =
  map (\ ((f, s), c) -> if c then f else s) (zip xs choice)

task5' :: [(a, a)] -> [Bool] -> [a]
task5' xs choice =
  [if c then f else s | ((f, s), c) <- zip xs choice]
