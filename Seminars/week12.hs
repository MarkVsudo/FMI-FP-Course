import Data.List


main :: IO()
main = do
  print $ pruneTree bt0 1
  print $ pruneTree bt1 1
  print $ pruneTree bt2 1
  print $ pruneTree bt3 1
  print $ generateNum t1 1 -- ➝ 3
  print $ generateNum t1 2 -- ➝ 27
  print $ generateNum t1 3 -- ➝ 4
  print $ generateNum t2 1 -- ➝ 1
  print $ generateNum t2 2 -- ➝ 0
  print $ generateNum t2 3 -- ➝ 6
  print $ generateNum' t1 1 -- ➝ 3
  print $ generateNum' t1 2 -- ➝ 27
  print $ generateNum' t1 3 -- ➝ 4
  print $ generateNum' t2 1 -- ➝ 1
  print $ generateNum' t2 2 -- ➝ 0
  print $ generateNum' t2 3 -- ➝ 6
  print $ redundant [[1,2],[1]] -- ➝ [2]
  print $ redundant [[1,2],[1,3],[2,3]] -- ➝ [1,2,3]
  print $ redundant [[1,2],[1,3],[2,3],[3],[4],[5],[3,4,5]] -- ➝ [1,2]
  print $ redundant [[1,2],[1,3],[3],[4,5],[5]] -- ➝ [1,2,4]


data BTree = Empty | Node Int BTree BTree
  deriving (Eq, Show)

bt0, bt1, bt2 :: BTree
bt0 = Node 6 Empty Empty
bt1 = Node 1 Empty
             (Node 0 (Node 0 Empty Empty)
                     (Node 1 Empty Empty))
bt2 = Node 1 (Node 0 (Node 0 Empty Empty)
                     (Node 0 Empty Empty))
             (Node 1 (Node 0 Empty Empty)
                     (Node 1 Empty Empty))
bt3 = Node 1 (Node 1 (Node 1 (Node 0 Empty Empty)
                             Empty)
                     (Node 1 Empty Empty))
             (Node 0 (Node 0 Empty Empty)
                     (Node 1 Empty Empty))

contains :: BTree -> Int -> Bool
contains Empty _ = False
contains (Node v lt rt) n =
  v == n || contains lt n || contains rt n

pruneTree :: BTree -> Int -> BTree
pruneTree Empty _ = Empty
pruneTree bt@(Node v lt rt) n =
  if not (contains bt n)
    then Empty
    else Node v (pruneTree lt n) (pruneTree rt n)


t1, t2 :: BTree
t1 = Node 6 (Node 3 (Node 2 Empty Empty)
                    (Node 5 (Node 4 Empty Empty)
                            Empty))
            (Node 8 (Node 7 Empty Empty)
                    (Node 9 Empty Empty))
t2 = Node 4 (Node 1 Empty
                    (Node 3 Empty Empty))
            (Node 5 Empty
                    (Node 7 (Node 6 Empty Empty)
                            Empty))

generateNum :: BTree -> Int -> Int
generateNum bt k =
  dsToNum $ if null ds then [0] else ds
  where
    getNumsList Empty _ = []
    getNumsList (Node _ (Node v _ _) _) 1 = [v]
    getNumsList (Node _ lt rt) k =
      getNumsList lt (k - 1) ++ getNumsList rt (k - 1)
    
    ds = getNumsList bt k

    dsToNum = foldr1 (\ res d -> res * 10 + d)

generateNum' :: BTree -> Int -> Int
generateNum' bt k =
  foldl (\ res d -> res * 10 + d) 0 (getNumsList bt k)
  where
    getNumsList Empty _ = []
    getNumsList (Node _ (Node v _ _) _) 1 = [v]
    getNumsList (Node _ lt rt) k =
      getNumsList lt (k - 1) ++ getNumsList rt (k - 1)

redundant :: [[Int]] -> [Int]
redundant ids = filter isRedundant products
  where
    countUsers = length . (filter (not . null))
    initialCount = countUsers ids
    products = nub $ foldl (++) [] ids
    removeProduct id = map (delete id) ids
    isRedundant id =
      countUsers (removeProduct id) == initialCount