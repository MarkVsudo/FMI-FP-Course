Продукт се представя с наредена двойка от вида (име, цена). Наличността в даден магазин се представя със списък от продукти.
`type Product = (String,Double)`
`type StoreAvailability = [Product]`

**Задача 1.** Да се напише на Haskell функция `closestToAverage :: StoreAvailability -> String`, която намира името на продукта, чиято цена е най-близка до средната цена за всички продукти. Ако има повече от един такъв продукт, функцията да връща името на кой да е от намерените.

Пример:
`store1 = [("bread",1),("milk",2.5),("lamb",10),("cheese",5),("butter",2.3)]`
`closestToAverage store1 ➝ "cheese"`

**Задача 2.** Да се напише на Haskell функция `cheaperAlternative :: StoreAvailability -> Int`, която намира броя на продуктите, за които има продукт със същото име, но по-ниска цена.

Пример:
`store2 = [("bread",1),("cheese",2.5),("bread",1),("cheese",5),("butter",2.3)]`
`cheaperAlternative store2 ➝ 1`

---

**Задача 3.** Нека са дефинирани следните типове:
`type Student = String -- име на ученик`
`type Subject = String -- име на предмет`
`type Note = Double -- оценка`
`-- Запис за ученик, съдържащ име на ученик, учебен предмет и оценката на`
`-- ученика по дадения предмет.`
`type Record = (Student, Subject, Note)`
Дефинирайте функцията `hardestSubject :: [Record] -> Subject`, която получава списък от записи за учениците от даден клас и връща името на предмета с най-ниска средна оценка за този клас.

---

**Задача 4.** Посещаемостта на ученик по предмет може да се представи чрез списък с елементи от алгебричен тип, който определя дали студент е пропуснал часа (`Absent`), закъснял е за часа (`Late`) или е присъствал в часа (`Present`). За да може да получи срочна оценка, ученикът не може да пропусне повече от `n` часа и не може да закъснее повече от `k` пъти подред. Да се дефинира функция от по-висок ред `canPass :: Criterion -> (StudentRecord -> Bool)`, която приема двуелементен вектор, представящ максималния разрешен брой пропуснати часове и поредни закъснения, които може да има даден ученик, и връща функция, която за дадена посещаемост на ученик връща булева стойност, определяща дали този ученик може да получи срочна оценка.

Примери:
`cP = canPass (1,2)`
`type Misses = Int`
`type Lates = Int`
`type Criterion = (Misses, Lates)`
`data Attendance = Absent | Late | Present deriving (Eq, Show)`
`type StudentRecord = [Attendance]`
`cP [Present, Late, Present, Absent, Present, Present, Present,<span> </span>``Absent] ➝ False`
`cP [Present, Late, Present, Late, Present, Late, Present, Absent,<span> </span>``Late, Present] ➝ True`
`cP [Present, Late, Present, Late, Late, Late, Present, Present,<span> </span>``Absent, Present] ➝ False`

---

**Задача 5.** Разглеждаме непразен списък `fs` от едноаргументни функции `[f1, f2, … , fn]`, всяка от които е от тип `Int -> Int`. Дефинирайте функция `getOddCompositionValue :: [Int -> Int] -> (Int -> Int)`, която при подаден такъв списък `fs` връща като резултат функция, чиято стойност за всяко цяло число `x` е равна на стойността на композицията на функциите с нечетни поредни номера от `fs` приложени към `x`, както следва `f1(f3( … (x) … ))`.

Пример:
`getOddCompositionValue [(\ x -> x + 1),(\ x -> x * 2),(\ x -> x - 1), (\ x -> x `div ` 2)] 2 ➝ 2`

`getOddCompositionValue [(\ x -> x + 1),(\ x -> x * 2),(\ x -> x * 10), (\ x -> x `div ` 2)] 2 ➝ 21`
