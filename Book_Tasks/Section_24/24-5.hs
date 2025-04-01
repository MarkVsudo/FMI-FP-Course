-- Да се дефинира функция histogram, която по символен низ s връща
-- списък от двойки (ci, ni), където ci са различните символи от s, а ni
-- е броя на срещания на ci в s. Например, histogram ”abracadabra” →
-- [(a, 5),(b, 2),(r, 2),(c, 1),(d, 1)]. Използвайте помощни функции.

import Data.List

histogram :: String -> [(Char, Int)]

histogram str = [(head g, length g) | g <- group (sort str)]

