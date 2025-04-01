-- Да се дефинира функция pack l :: [a] → [[a]], която получава списък и
-- връща списък от списъци, като всеки от тях съдържа всички последо-
-- вателни еднакви елементи на входния списък.
-- Например, pack [1, 1, 1, 2, 2, 3, 4, 4, 4, 4] → [[1, 1, 1], [2, 2], [3], [4, 4, 4, 4]].

import Data.List

pack :: Eq a => [a] -> [[a]]

pack l = group l
