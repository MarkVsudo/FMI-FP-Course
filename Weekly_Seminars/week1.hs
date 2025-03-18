main = do
  print 1
  print "123"
  print (show 123)
  print (f1 2)
  print $ f1 2
  print (f3 1 2 3)
  print $ f3 1 2 3
  print ((f3 1) 2 3)
  print $ fact 5
  print (fib 5)
  print (fibIter 5)
  print (fibIter 50)
  

f1 x = 2 * (f2 x)

f2 x = x + 5

-- Int
-- Integer
-- Double
-- Char
-- String === [Char]
-- Bool: True или False

f3 a b c = a * (b + c)

f4 :: Double -> Double
f4 x = 5 * x

fact n =
  if n == 0
    then 1
    else n * fact (n - 1)

fib n =
  if n == 0 then 0
  else if n == 1 then 1
  else fib (n - 2) + fib (n - 1)

helper cur next n =
  if n == 0 then cur
  else helper next (cur + next) (n - 1)

fibIter n = helper 0 1 n