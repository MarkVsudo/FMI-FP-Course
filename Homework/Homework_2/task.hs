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

-- Task 1)
instance Show Animal where
  show (Animal s id d h) = s ++ "(" ++ show id ++ "," ++ show d ++ "," ++ show h ++ ")"

-- Task 2)
updateDiet :: [Animal] -> AnimalId -> Diet -> [Animal]
updateDiet zoo id newDiet =
  [if animalId a == id then a { diet = newDiet } else a | a <- zoo]

-- Task 3)
feedAnimal :: [Animal] -> Int -> AnimalId -> [Animal]
feedAnimal zoo k id =
  map (\a -> if animalId a == id then a { hungerLevel = hungerLevel a + k } else a) zoo

-- Task 4)
feedIfHungry :: [Animal] -> Int -> [AnimalId] -> [Animal]
feedIfHungry zoo k ids =
  map feedIf zoo
  where
    feedIf a
      | animalId a `elem` ids && hungerLevel a < 5 = a { hungerLevel = hungerLevel a + k }
      | otherwise = a


-- Task 5)
findAnimalById :: [Animal] -> AnimalId -> Maybe Animal
findAnimalById [] _ = Nothing
findAnimalById (a:as) id
  | animalId a == id = Just a
  | otherwise = findAnimalById as id

