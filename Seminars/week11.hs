import Data.List

main :: IO()
main = do
  print $ countLetters "Hello World" -- ➝ 5
  print $ countLetters " haskell is great " -- ➝ 2
  print $ countLetters "Information Systems 2023" -- ➝ 7
  print $ groupEquals ["eat","tea","tan","ate","nat","bat"] -- ➝ [["bat"],["nat","tan"],["ate","eat","tea"]]
  print $ groupEquals [""]  -- ➝ [[""]]
  print $ groupEquals ["a"] -- ➝ [["a"]]
  print $ transposeMat [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
  print $ fn (== 5)  -- ➝ [[0,2,3,4],[0,6,7,8],[0,10,11,12]]
  print $ fn (>= 11) -- ➝ [[1,2,0,0],[5,6,0,0],[9,10,0,0]]
  print $ fn (> 20)  -- ➝ [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
  where
    fn = resetMatrix [[1,2,3,4],[5,6,7,8],[9,10,11,12]]

-- String === [Char]
countLetters :: String -> Int
countLetters = length . (!! 1) . reverse . words

groupEquals :: [String] -> [[String]]
groupEquals =
   (map (map fst)) .
    (groupBy (\ (_, a) (_, b) -> a == b)) .
    (sortOn snd) .
    (map (\ str -> (str, (sort str))))

{-
[[1, 2, 3, 4],
 [5, 6, 7, 8],
 [9,10,11,12]]
-}
transposeMat :: [[Int]] -> [[Int]]
transposeMat [] = []
transposeMat ([]:_) = []
transposeMat mat =
  (map head mat) : (transposeMat (map tail mat))

resetMatrix :: [[Int]] -> ((Int -> Bool) -> [[Int]])
resetMatrix mat = \ p ->
  transposeMat $
    map (\ row -> if any p row
                    then [0 | _ <- row]
                    else row) $
    transposeMat mat