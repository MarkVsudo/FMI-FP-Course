-- Да се дефинира функция, която по двойка (x, y) координати на точка от
-- равнината намира на кой квадрант принадлежи точката. Да се разгледат
-- случаите, когато точката принадлежи на някоя от координатните оси
-- или съвпада с центъра на координатната система.

main :: IO()
main = do
  print (findQuadrant (-2) 1)   
  print (findQuadrant 3 (-4))   
  print (findQuadrant 0 0)      
  print (findQuadrant 0 5)      
  print (findQuadrant 6 0)      

findQuadrant x y = 
  if x > 0 && y > 0 then "First Quadrant"
  else if x < 0 && y > 0 then "Second Quadrant"
  else if x < 0 && y < 0 then "Third Quadrant"
  else if x > 0 && y < 0 then "Fourth Quadrant"
  else if x == 0 && y /= 0 then "On Y-axis"
  else if x /= 0 && y == 0 then "On X-axis"
  else "On Center"
