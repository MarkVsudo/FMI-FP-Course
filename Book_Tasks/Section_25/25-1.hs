-- Дефинирайте функция say, която по едноцифрено цяло число връща
-- неговото наменование. Например, say 3 → ”three”.

numsStrs = ["zero"
        ,"one"
        ,"two"
        ,"three"
        ,"four"
        ,"five"
        ,"six"
        ,"seven"
        ,"eight"
        ,"nine"]

say :: Int -> String
say n = if n < 0 || n > 9 
        then "Invalid output" 
        else numsStrs !! n


