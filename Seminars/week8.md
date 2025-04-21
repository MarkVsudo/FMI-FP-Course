**Задача 1.** Да се дефинира функция mapTree :: (a -> b) -> BTree a -> BTree b, която прилага дадена функция f към всеки възел на дървото.

**Задача 2.** Нека имаме типът за цвят  *data Color = Red | Green | Blue deriving (Read, Show, Eq)* . Дефинирайте функция  *maxDepthBlueNode :: BTree Color -> Int* , която намира дълбочината на най-дълбокия (най-отдалечения от корена) връх с цвят *Blue *на дадено двоично дърво от тип  *Color* .

Примерно дърво:

```
colorTree :: BTree Color                                            --            Blue
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty)     --           /    \
                      (Node Red (Node Blue (Node Green Empty Empty) --        Red      Red
                                           (Node Red Empty Empty))  --        /        /  
                                Empty)                              --     Green     Blue  
                                                                    --               /   \
                                                                    --            Green  Red
```

**Задача 3.** Дефинирайте функция  *maxDepthNode :: BTree Color -> Color -> Int* , която намира дълбочината на най-дълбокия връх с цвят, подаден като аргумент, на дадено двоично дърво от тип  *Color* .

**Задача 4.** Да се дефинира алгебричен тип  *NTree а* , който да представлява дърво с произволен брой наследника на всеки възел. За него да се дефинира фунцкия  *size* , която брои елементите му
Примерно дърво:

```
nTree1 :: NTree Int                               --       1
nTree1 = NNode 1 [(NNode 2 [(NNode 3 [NEmpty]),   --      / \
                            (NNode 4 [NEmpty]),   --     2   6
                            (NNode 5 [NEmpty])]), --    /|\  |
                  (NNode 6 [(NNode 7 [NEmpty])])] --   3 4 5 7
```

**Задача 5.** Казваме, че едно дърво е грациозно, ако абсолютните стойности на разликите между стойностите на всеки елемент и бащиния му са четни. Да се дефинира функция  *isGraceful :: NTree Int -> Bool* , която приема  *n* -арно дърво и проверява дали то е грациозно.
