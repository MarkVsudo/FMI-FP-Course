-- Да се дефинира функция, която намира лицето на триъгълник по даде-
-- ни: а) дължини на страна и височина към нея; б) три страни.

main :: IO()
main = do
  print (findTriangleArea1 3 5)  
  print (findTriangleArea2 7 8 5)  

-- a)
findTriangleArea1 a ha = a * ha / 2  

-- b)
findTriangleArea2 a b c =
    let r = (a + b + c) / 2  
    in sqrt (r * (r - a) * (r - b) * (r - c))