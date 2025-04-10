type Species = String
type AnimalId = Int

data Diet = Herbivore | Carnivore | Omnivore
  deriving (Show, Eq)

data Animal = Animal
  { species :: Species
  , animalId :: AnimalId
  , diet :: Diet
  , hungerLevel :: Int
  } deriving (Eq)

zoo = [Animal "Lion" 1 Carnivore 3, Animal "Elephant" 2 Herbivore 3, Animal "Bear" 3 Omnivore 6, Animal "Turtle" 4 Herbivore 2]

-- Task 1)
-- Дефинира се инстанция на класа Show за типа Animal.
-- При извикване на show върху животно, то ще се представя като низ от вида:
-- "<species>(<id>,<diet>,<hungerLevel>)"
instance Show Animal where
  show (Animal s id d h) = s ++ "(" ++ show id ++ "," ++ show d ++ "," ++ show h ++ ")"

-- Task 2)
-- Функцията updateDiet обновява диетата на животно по зададен ID.
-- Използва се List Comprehension, като се връща нов списък от животни.
-- Само животното с посочения ID променя своята диета на newDiet.
updateDiet :: [Animal] -> AnimalId -> Diet -> [Animal]
updateDiet zoo id newDiet =
  [if animalId a == id then a { diet = newDiet } else a | a <- zoo]

-- Task 3)
-- Функцията feedAnimal увеличава нивото на глад на животно с даден ID с k единици.
-- Използва се map за обхождане на всички животни в zoo и се връща нов списък.
feedAnimal :: [Animal] -> Int -> AnimalId -> [Animal]
feedAnimal zoo k id =
  map (\a -> if animalId a == id then a { hungerLevel = hungerLevel a + k } else a) zoo

-- Task 4)
-- Функцията feedIfHungry увеличава нивото на глад с k единици
-- само за гладни животни (с глад < 5), чиито ID са в списъка ids.
-- Използва се map за обхождане на животните и се връща нов списък.
feedIfHungry :: [Animal] -> Int -> [AnimalId] -> [Animal]
feedIfHungry zoo k ids =
  map feedIf zoo
  where
    feedIf a
      | animalId a `elem` ids && hungerLevel a < 5 = a { hungerLevel = hungerLevel a + k }
      | otherwise = a

-- Task 5)
-- Функцията findAnimalById търси животно в списък по даден ID.
-- Използва се рекурсия. Ако се намери животно с този ID – връща Just <Animal>,
-- ако не – връща Nothing.
findAnimalById :: [Animal] -> AnimalId -> Maybe Animal
findAnimalById [] _ = Nothing
findAnimalById (a:as) id
  | animalId a == id = Just a
  | otherwise = findAnimalById as id

