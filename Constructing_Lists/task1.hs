-- Да се съставят следните списъци:
-- а) Първите n четни числа;

firstEvenNumbers :: Int -> [Int]
firstEvenNumbers n = helper 1 [0]
  where
    helper i res =
      if i == n then reverse res
      else helper (i + 1) ((head res + 2):res)

firstEvenNumbers' :: Int -> [Int]
firstEvenNumbers' n = [2 * i | i <- [0..n-1]]

firstEvenNumbers'' :: Int -> [Int]
firstEvenNumbers'' n = take n [0,2..]