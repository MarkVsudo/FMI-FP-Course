main :: IO()
main = do
  print $ listOfIndexes 3 [1,2,3,4,3,5,3,2,1] -- → [2,4,6]
  print $ listOfIndexes 4 [1,2,3,2,1,2,3,2,1] -- → []
  print $ listOfIndexes' 3 [1,2,3,4,3,5,3,2,1] -- → [2,4,6]
  print $ listOfIndexes' 4 [1,2,3,2,1,2,3,2,1] -- → []
  print $ listOfIndexes'' 3 [1,2,3,4,3,5,3,2,1] -- → [2,4,6]
  print $ listOfIndexes'' 4 [1,2,3,2,1,2,3,2,1] -- → []
  print $ factorize 13  -- → [13]
  print $ factorize 152 -- → [2,2,2,19]
  print $ factorize 123 -- → [3,41]
  print $ (averageFunction [(+1),(**0.5),(2**)]) 2 -- → 2.804738
  print $ (averageFunction' [(+1),(**0.5),(2**)]) 2 -- → 2.804738
  print bt
  print $ singleCousin bt


listOfIndexes :: Int -> [Int] -> [Int]
listOfIndexes n xs = [i | (i, x) <- zip [0..] xs, x == n]

listOfIndexes' :: Int -> [Int] -> [Int]
listOfIndexes' n xs = map (\ (i, _) -> i) (filter (\ (_, x) -> x == n) (zip [0..] xs))

listOfIndexes'' :: Int -> [Int] -> [Int]
listOfIndexes'' n = (map fst) . (filter ((== n) . snd)) . (zip [0..])

factorize :: Int -> [Int]
factorize n = helper 2 n
  where
    helper _ 1 = []
    helper d k =
      if k `mod` d == 0
        then d : helper d (k `div` d)
        else helper (d + 1) k

averageFunction :: (Fractional a, Num a) => [(a -> a)] -> (a -> a)
averageFunction fs = \ x -> average [f x | f <- fs]
  where
    average xs = sum xs / fromIntegral (length xs)

averageFunction' :: (Fractional a, Num a) => [(a -> a)] -> (a -> a)
averageFunction' fs = \ x -> average (map ($ x) fs)
  where
    average xs = sum xs / fromIntegral (length xs)

data BTree = Empty | Node Int BTree BTree deriving (Eq, Show)

bt :: BTree
bt = Node 1 (Node 2 (Node 4 (Node 8 Empty Empty)
                            (Node 9 Empty Empty))
                    (Node 5 Empty
                            (Node 10 Empty Empty)))
            (Node 3 (Node 6 Empty
                            (Node 11 Empty Empty))
                    (Node 7 Empty
                            (Node 12 Empty Empty)))
--          1 
--       /     \ 
--      2       3
--     / \     / \
--    4   5   6    7 
--   / \   \   \    \ 
--  8   9   10  11   12

getChildren :: BTree -> [Int]
getChildren Empty                                = []
getChildren (Node _ Empty         Empty)         = []
getChildren (Node _ (Node v _ _)  Empty)         = [v]
getChildren (Node _ Empty         (Node v _ _))  = [v]
getChildren (Node _ (Node v1 _ _) (Node v2 _ _)) = [v1, v2]

getChildren' :: BTree -> [Int]
getChildren' Empty = []
getChildren' (Node _ lt rt) = getRoot lt ++ getRoot rt
  where
    getRoot Empty = []
    getRoot (Node v _ _) = [v]

singleCousin :: BTree -> [Int]
singleCousin Empty = []
singleCousin (Node _ lt rt) =
  (if length rChildren == 1 then lChildren else [])
    ++ (if length lChildren == 1 then rChildren else [])
    ++ singleCousin lt ++ singleCousin rt
  where
    lChildren = getChildren' lt
    rChildren = getChildren' rt