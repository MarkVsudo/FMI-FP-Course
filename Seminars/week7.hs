main :: IO ()
main = do
  print 7

-- data Maybe a = Nothing | Just a
type Scale = (Double, Double)

s1 :: Scale
s1 = (0, 0)

validate :: Scale -> Maybe Scale
validate (l, r) =
  if abs (l - r) > 50 || l > 100 || r > 100
    then Nothing
    else Just (l, r)

left :: Double -> Scale -> Maybe Scale
left x (l, r) = validate $ (l + x, r)

right :: Double -> Scale -> Maybe Scale
right x (l, r) = validate $ (l, r + x)

f1 :: Maybe Scale -> (Scale -> Maybe Scale) -> Maybe Scale
f1 scale f =
  case scale of
    Nothing -> Nothing
    Just s -> f s


-- https://haskell.mooc.fi/part1
-- Прочетете внимателно всичко до 6.6 (включително)

data BTree a = Empty | Node a (BTree a) (BTree a)
--    1
bt1 :: BTree Int
bt1 = Node 1 Empty Empty
--    1
--   / \
--  2   3
--     / \
--    4   5
bt2 :: BTree Int
bt2 = Node 1 (Node 2 Empty Empty)
             (Node 3 (Node 4 Empty Empty)
                     (Node 5 Empty Empty))
sumTree :: BTree Int -> Int
sumTree Empty = 0
sumTree (Node v lt rt) = v + sumTree lt + sumTree rt

countNodes :: BTree a -> Int
countNodes Empty = 0
countNodes (Node _ lt rt) = 1 + countNodes lt + countNodes rt

change :: Eq a => BTree a -> a -> a -> BTree a
change Empty          _ _ = Empty
change (Node v lt rt) p q =
  Node (if p == v then q else v) (change lt p q) (change rt p q)
-- change (Node 1 Empty (Node 2 Empty Empty)) 1 10
--   -> Node 10 Empty (Node 2 Empty Empty)