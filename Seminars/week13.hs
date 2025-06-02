import Data.Function
import Data.List


main :: IO()
main = do
  print $ highestCapital cs
  print $ election [b1, b2, b3] cl
  print $ minCount tr 4 -- → 1
  print $ minCount tr 2 -- → 3
  where
    cs = [ Country "a" "a1" [ City "a1" 100 10.2
                            , City "a2" 120 10.2
                            , City "a3" 90 10.2 ]
         , Country "b" "b2" [ City "b1" 100 10.2
                            , City "b2" 120 10.2
                            , City "b3" 90 10.2 ]
         , Country "c" "c3" [ City "c1" 100 10.2
                            , City "c2" 120 10.2
                            , City "c3" 90 10.2 ] ]
    cl = [("Kernighan","C"), ("Ritchie","C"), ("Stroustrup","C++"), ("Steele","Scheme"),("Sussman","Scheme"),("Church","Lambda"), ("Curry","Lambda")]
    b1 (name, specialty) = specialty == "Lambda" || last name == 'e'
    b2 (name, specialty) = name == "Church" || head specialty == 'C'
    b3 (name, specialty) = length name > 6 && specialty /= "C++"
    tr = (Node 2 (Node 4 (Node 4 Empty Empty) Empty) Empty)


type Name = String                                   -- име
type Capital = Name                                  -- столица
type AvgYearlyTemperature = Double                   -- средногодишна температура
type Elevation = Int                                 -- надморска височина
data City = City Name Elevation AvgYearlyTemperature -- град
  deriving (Read, Show)
data Country = Country Name Capital [City]           -- държава
  deriving (Read, Show)

highestCapital :: [Country] -> Name
highestCapital =
  getName . (maximumBy (compare `on` capitalHeight))
  where
    getName (Country n _ _) = n
    capitalHeight (Country _ capital cities) =
      head [e | (City n e _) <- cities, n == capital]


election :: [((String, String) -> Bool)] -> [(String, String)] -> [(String, Int)]
election bs cs =
  sortBy (compare `on` ((* (-1)) . snd)) [(n, count c) | c@(n, _) <- cs]
  -- sortBy (compare `on` ((* (-1)) . snd)) (zip (map fst cs) (map count cs))
  where
    count c = length (filter ($ c) bs)


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Node _ lt rt) =
  1 + countNodes lt + countNodes rt

height :: Tree a -> Int
height Empty = 0
height (Node _ lt rt) =
  1 + max (height lt) (height rt)

minCount :: Eq a => Tree a -> a -> Int
minCount Empty _ = 0
minCount t x = snd (minimum (helper t))
  where
    helper Empty = []
    helper t@(Node v lt rt) =
      (if v == x then [(height t, countNodes t)] else [])
        ++ helper lt ++ helper rt
  