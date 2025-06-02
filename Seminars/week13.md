**Задача 1.** Нека са дефинирани типовете:
`type Name = String                                   -- име`
`type Capital = Name                                  -- столица`
`type AvgYearlyTemperature = Double                   -- средногодишна температура`
`type Elevation = Int                                 -- надморска височина`
`data City = City Name Elevation AvgYearlyTemperature -- град`
` deriving (Read, Show)`
`data Country = Country Name Capital [City]           -- държава`
` deriving (Read, Show)`
Дефинирайте функция `highestCapital :: [Country] -> Name`, която получава като аргумент списък от държави и връща като резултат името на държавата от списъка с най-висока столица (столица с най-голяма надморска височина).

**Задача 2.** Обществото на информатиците провежда избори за съвет на старейшините. „Кандидатура“ е наредена двойка от низове: името на кандидата и неговата специалност, а „кандидатската листа“ е списък от кандидатури. Може да се гласува за произволен брой кандидати. Попълнените „бюлетини“ представляват предикати, които приемат кандидатура и връщат истина или лъжа, в зависимост от това дали се гласува за съответната кандидатура или не. Да се дефинира функция `election :: [((String, String) -> Bool)] -> [(String, String)] -> [(String, Int)]`, която връща сортиран в низходящ ред списък от наредени двойки от името на кандидата и броя на гласовете за него от бюлетините в реда, в който кандидатите се срещат в листата.
*Пример:*
`cl = [("Kernighan","C"), ("Ritchie","C"), ("Stroustrup","C++"), ("Steele","Scheme"),("Sussman","Scheme"),("Church","Lambda"), ("Curry","Lambda")]`
`b1 (name, specialty) = specialty == "Lambda" || last name == 'e'`
`b2 (name, specialty) = name == "Church" || head specialty == 'C'`
`b3 (name, specialty) = length name > 6 && specialty /= "C++"`
`election [b1, b2, b3] cl → [("Ritchie",3), ("Kernighan",2), ("Church",2), ("Stroustrup",1), ("Steele",1), ("Sussman",1), ("Curry",1)]`

**Задача 3.** Нека е дефиниран алгебричен тип, представящ двоично дърво, както следва:
`data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)`
Да се дефинира функция `(minCount bTree x)` от тип `minCount :: (Eq a) => Tree a -> a -> Int`, която намира броя на върховете (включително корена) на най-ниското поддърво на дадено двоично дърво `bTree` със стойност в корена, равна на `x`.
*Пример:*
`tr = (Node 2 (Node 4 (Node 4 Empty Empty) Empty) Empty)`
`minCount tr 4 → 1`
`minCount tr 2 → 3`
