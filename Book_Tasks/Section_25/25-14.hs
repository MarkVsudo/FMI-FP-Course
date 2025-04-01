-- Да се дефинира функция removeDuplicates l, която премахва всички
-- повторения на елементите на списъка l.

import Data.List (nub)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates l = nub l