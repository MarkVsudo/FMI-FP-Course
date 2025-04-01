-- Да се дефинира функция, която проверява дали даден низ е палиндром,
-- т.е. дали се е еднакъв при четене от ляво на дясно и от дясно на ляво.

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome str = if head str == last str
                   then isPalindrome $ init (tail str)
                   else False

isPalindrome' :: String -> Bool
isPalindrome' str = str == reverse str

