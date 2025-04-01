-- Да се дефинира функция groupsof l x, която разделя списъка l на гру-
-- пи от по x елемента.
-- Например, groupsof [1, 2, 3, 4, 5, 6, 7, 8] 3 → [[1, 2, 3], [4, 5, 6], [7, 8]].

groupsof :: [Int] -> Int -> [[Int]]
groupsof [] _ = []
groupsof l x = take x l : groupsof (drop x l) x

