**Задача 1.** Нека е дадена следната дефиниция за двоично дърво:
`data BTree = Empty | Node Int BTree BTree deriving (Eq, Show)`
Дефинирайте функция `pruneTree :: BTree -> Int -> BTree`, която получава двоично дърво `bt` и стойност `n`. Функцията трябва да върне като резултат ново двоично дърво със същата структура като `bt`, но в което са премахнати всички поддървета, които не съдържат възел със стойност `n`. Всяко дърво е поддърво на себе си.
*Примери:*
`bt0 = Node 6 Empty Empty`
`bt1 = Node 1 Empty`
`             (Node 0 (Node 0 Empty Empty)`
`                     (Node 1 Empty Empty))`
`bt2 = Node 1 (Node 0 (Node 0 Empty Empty)`
`                     (Node 0 Empty Empty))`
`             (Node 1 (Node 0 Empty Empty)`
`                     (Node 1 Empty Empty))`
`bt3 = Node 1 (Node 1 (Node 1 (Node 0 Empty Empty)`
`                             Empty)`
`                     (Node 1 Empty Empty))`
`             (Node 0 (Node 0 Empty Empty)`
`                     (Node 1 Empty Empty))`
`pruneTree bt0 1 ➝ Empty`
`pruneTree bt1 1 ➝ Node 1 Empty`
`                          (Node 0 Empty`
`                                  (Node 1 Empty Empty))`
`pruneTree bt2 1 ➝ Node 1 Empty`
`                          (Node 1 Empty`
`                                  (Node 1 Empty Empty))`
`pruneTree bt3 1 ➝ Node 1 (Node 1 (Node 1 Empty Empty)`
`                                  (Node 1 Empty Empty))`
`                          (Node 0 Empty`
`                                  (Node 1 Empty Empty))`

**Задача 2.** Нека е дефиниран следният алгебричен тип, описващ двоично дърво:
`data BTree = Empty | Node Int BTree BTree deriving (Eq, Show)`
Да се дефинира функция `generateNum :: BTree -> Int -> Int`, която получава два параметъра: двоично дърво `bt` и номер на ниво `k > 0`. Двоичното дърво `bt` има само едноцифрени числа във всеки от възлите. Функцията `generateNum` трябва да върне цяло число, съставено от всички цифри, които се намират на ниво `k` в `bt` и са във възел, който е корен на ляво поддърво. Цифрите на числото са подредени от ляво надясно. Коренът на `bt` е на ниво `0`. Ако не съществува ниво `k` или няма възли, които отговарят на изискванията, то върнете резултат `0`.
*Примери:*
`t1 :: BTree`
`t1 = Node 6 (Node 3 (Node 2 Empty Empty)`
`                    (Node 5 (Node 4 Empty Empty)`
`                            Empty))`
`            (Node 8 (Node 7 Empty Empty)`
`                    (Node 9 Empty Empty))`
`t2 :: BTree`
`t2 = Node 4 (Node 1 Empty`
`                    (Node 3 Empty Empty))`
`            (Node 5 Empty`
`                    (Node 7 (Node 6 Empty Empty)`
`                            Empty))`

`generateNum t1 1 ➝ 3`
`generateNum t1 2 ➝ 27`
`generateNum t1 3 ➝ 4`
`generateNum t2 1 ➝ 1`
`generateNum t2 2 ➝ 0`
`generateNum t2 3 ➝ 6`

**Задача 3.** Голям онлайн магазин иска да съкрати предлаганите продукти, но без да губи потребители. За целта трябва да се дефинира функция `redundant :: [[Int]] -> [Int]`, която получава списък от списъци с идентификатори на продукти (цели числа), които са закупили различни потребители. Функцията `redundant` трябва да върне нов списък с идентификаторите на продуктите, за всеки от които е вярно, че ако се премахне, то няма да се промени броят на потребителите.

*Примери:*

`redundant [[1,2],[1]] ➝ [2]`
ако се премахне продукт 1, броят на потребителите ще се намали;
ако се премахне продукт 2, броят на потребителите се запазва

`redundant [[1,2],[1,3],[2,3]] ➝ [1,2,3]`
който и продукт да се премахне, броят на потребителите ще се запази

`redundant [[1,2],[1,3],[2,3],[3],[4],[5],[3,4,5]] ➝ [1,2]`
продукти 3, 4 и 5 не могат да се премахнат, защото има потребители, които са
закупили само тях

`redundant [[1,2],[1,3],[3],[4,5],[5]] ➝ [1,2,4]`
