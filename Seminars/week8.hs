main :: IO ()
main = do
  print $ mapTree (* 2) bt2


data BTree a = Empty | Node a (BTree a) (BTree a)
  deriving (Eq, Show)

bt1 :: BTree Int                           --    1
bt1 = Node 1 (Node 2 Empty Empty)          --   / \
             (Node 3 (Node 4 Empty Empty)  --  2   3
                     (Node 5 Empty Empty)) --     / \
                                           --    4   5

bt2 :: BTree Int                           --     3
bt2 = Node 3 (Node 2 (Node 1 Empty Empty)  --    / \
                     Empty)                --   2   5
             (Node 5 (Node 4 Empty Empty)  --  /   / \
                     (Node 6 Empty Empty)) -- 1   4   6


mapTree :: (a -> b) -> BTree a -> BTree b
mapTree _ Empty          = Empty
mapTree f (Node v lt rt) =
  Node (f v) (mapTree f lt) (mapTree f rt)

data Color = Red | Green | Blue deriving (Read, Show, Eq)

colorTree :: BTree Color                                            --            Blue
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty)     --           /    \
                      (Node Red (Node Blue (Node Green Empty Empty) --        Red      Red
                                           (Node Red Empty Empty))  --        /        /  
                                Empty)                              --     Green     Blue  
                                                                    --               /   \
                                                                    --            Green  Red

maxDepthBlueNode :: BTree Color -> Int
maxDepthBlueNode Empty = -10000
maxDepthBlueNode (Node Blue lt rt) =
  max 0
      (1 + max (maxDepthBlueNode lt)
               (maxDepthBlueNode rt))
maxDepthBlueNode (Node _ lt rt) =
  1 + max (maxDepthBlueNode lt)
          (maxDepthBlueNode rt)

maxDepthBlueNode' :: BTree Color -> Int
maxDepthBlueNode' = helper 0
  where
    helper _ Empty = -1
    helper i (Node Blue lt rt) =
      maximum [i, helper (i + 1) lt, helper (i + 1) rt]
    helper i (Node _ lt rt) =
      max (helper (i + 1) lt) (helper (i + 1) rt)

enumTree :: BTree a -> BTree (a, Int)
enumTree = helper 0
  where
    helper _ Empty = Empty
    helper i (Node v lt rt) =
      Node (v, i) (helper (i + 1) lt) (helper (i + 1) rt)

inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node v lt rt) =
  inorder lt ++ [v] ++ inorder rt

maxDepthBlueNode'' :: BTree Color -> Int
maxDepthBlueNode'' bt =
  maximum [i | (Blue, i) <- inorder (enumTree bt)]

maxDepthNode :: BTree Color -> Color -> Int
maxDepthNode bt c =
  maximum [i | (cv, i) <- inorder (enumTree bt), cv == c]

data NTree a = NEmpty | NNode a [NTree a]
  deriving (Eq, Show)

nTree1 :: NTree Int                               --       1
nTree1 = NNode 1 [(NNode 2 [(NNode 3 [NEmpty]),   --      / \
                            (NNode 4 [NEmpty]),   --     2   6
                            (NNode 5 [NEmpty])]), --    /|\  |
                  (NNode 6 [(NNode 7 [NEmpty])])] --   3 4 5 7

size :: NTree a -> Int
size NEmpty = 0
size (NNode _ ts) = 1 + sum (map size ts)
-- size (NNode _ ts) = 1 + sum [size t | t <- ts]