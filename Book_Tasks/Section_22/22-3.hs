-- Да се дефинира функция, която има стойност истина, ако посоченото
-- условие е вярно и стойност - лъжа, в противен случай:
-- а) цялото число p се дели на 4 или на 7;
-- б) уравнението ax2 + bx + c = 0(a ̸= 0) няма реални корени;
-- в) точка с координати (a, b) лежи във вътрешността на кръг с радиус 5 и център (0, 1); 
-- г) точка с координати (a, b) лежи извън кръга с център (c, d) и радиус f;
-- д) точка принадлежи на частта от кръга с център (0, 0) и радиус 5 в трети квадрант;
-- е) точка принадлежи на венеца с център (0, 0) и радиуси 5 и 10;
-- ж) x принадлежи на отсечката [0, 1];
-- з) x е равно на max {a, b, c};
-- и) x е различно от max { a, b, c};
-- к) нито едно от числата a, b и c не е положително;
-- л) цифрата 7 влиза в записа на положителното трицифрено число p;
-- м) цифрите на трицифреното число m са различни;
-- н) поне две от цифрите на трицифреното число m са равни помежду си;
-- о) цифрите на трицифреното естествено число x образуват строго растяща или строго намаляваща редица;
-- п) десетичните записи на трицифрените естествени числа x и y са симетрични;
-- р) естественото число x, за което се знае, че е по-малко от 23, е просто.

main :: IO()
main = do
  -- а)
  print "1)"
  print (isDivisible 20)
  print (isDivisible 28)
  -- б) 
  print "2)"
  print (quadraticEquationSolution 1 1 1)
  print (quadraticEquationSolution 2 3 1)
  -- в) 
  print "3)"
  print (isInFixedCircle 4 3)
  print (isInFixedCircle 7 2)
  -- г) 
  print "4)"
  print (isOutsideOfCircle 10 10 0 0 5)
  print (isOutsideOfCircle 2 2 0 0 5)
  -- д) 
  print "5)"
  print (isInThirdQuadrant (-3) (-4))
  print (isInThirdQuadrant (-4) (-4))
  -- е) 
  print "6)"
  print (isInAnnulus 6 6)
  print (isInAnnulus 3 3)
  -- ж) 
  print "7)"
  -- ?
  -- з) 
  print "8)"
  print (isMax 3 2 1 3)
  print (isMax 4 2 1 3)
  -- и) 
  print "9)"
  print (isNotMax 3 2 1 3)
  print (isNotMax 4 2 1 3)
  -- к) 
  print "10)"
  print (areNotPositiveNums 2 3 4)
  print (areNotPositiveNums (-2) (-3) (-4))
  -- л) 
  print "11)"
  print (numContainsSeven 120)
  print (numContainsSeven 172)
  -- м) 
  print "12)"
  print (areDigitsDifferent 121)
  print (areDigitsDifferent 123)
  -- н) 
  print "13)"
  print (atLeastTwoEqualDigits 111)
  print (atLeastTwoEqualDigits 123)
  -- о)  
  print "14)"
  print (isStrictSequence 132)
  print (isStrictSequence 123)
  print (isStrictSequence 321)
  -- п)
  print "15)"
  print (areNumsSymmetrical 123 231)
  print (areNumsSymmetrical 123 321)
  -- р)
  print "16)"

-- a)
isDivisible x = x `mod` 4 == 0 && x `mod` 7 == 0
-- б) 
quadraticEquationSolution a b c = b^2 - 4*a*c <= 0
-- в) 
-- (x−x0)^2+(y−y0)^2≤r^2 --> circle formula
isInFixedCircle a b = (a^2 + (b - 1)^2) <= 25
-- г) 
isOutsideOfCircle a b c d f = (a - c)^2 + (b - d)^2 > f^2
-- д) 
isInThirdQuadrant a b = (a < 0 && b < 0) && (a^2 + b^2 <= 25)
-- е) 
-- r1^2≤a^2+b^2≤r2^2 --> annulus formula
isInAnnulus a b = (25 <= a^2 + b^2) && (a^2 + b^2 <= 100)
-- ж) 
-- ?
-- з) 
isMax x a b c = x == max a (max b c)
-- и) 
isNotMax x a b c = x /= max a (max b c)
-- к) 
areNotPositiveNums a b c = a < 0 && b < 0 && c < 0
-- л) 
numContainsSeven p = if p == 0 then False 
  else if p `mod` 10 == 7 then True
  else numContainsSeven (p `div` 10)  
-- м) 
areDigitsDifferent :: Int -> Bool
areDigitsDifferent m =
    let firstDigit = m `mod` 10        
        secondDigit = (m `div` 10) `mod` 10  
        thirdDigit = m `div` 100       
    in firstDigit /= secondDigit && firstDigit /= thirdDigit && secondDigit /= thirdDigit
-- н) 
atLeastTwoEqualDigits :: Int -> Bool
atLeastTwoEqualDigits m =
    let firstDigit = m `mod` 10        
        secondDigit = (m `div` 10) `mod` 10  
        thirdDigit = m `div` 100       
    in firstDigit == secondDigit || firstDigit == thirdDigit || secondDigit == thirdDigit 
-- о) 
isStrictSequence m = 
  let firstDigit = m `mod` 10
      secondDigit = (m `div` 10) `mod` 10
      thirdDigit = m `div` 100
  in (firstDigit < secondDigit && secondDigit < thirdDigit) || (firstDigit > secondDigit && secondDigit > thirdDigit)   
-- п) 
areNumsSymmetrical x y = 
   let firstDigitX = x `mod` 10
       secondDigitX = (x `div` 10) `mod` 10
       thirdDigitX = x `div` 100
       firstDigitY = y `mod` 10
       secondDigitY = (y `div` 10) `mod` 10
       thirdDigitY = y `div` 100
    in thirdDigitX == firstDigitY && secondDigitX == secondDigitY && firstDigitX == thirdDigitY  
-- р) 
-- р) естественото число x, за което се знае, че е по-малко от 23, е просто.


