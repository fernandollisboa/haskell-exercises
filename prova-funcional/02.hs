-- Encontrar floor e ceil de um número x dentro de um array a. O número x pode não estar no array a. O
-- myFloor(x) é o número do array a que é menor que x e que mais se aproxima de x (pode exis r mais de um
-- número menor que x, o myFloor é o maior deles). Dualmente, o ceil(x) é o número do array a que é maior que
-- x e que mais se aproxima de x (pode exis r maisde um número maior do que x, o ceil é o menor deles).
arr = [10, 22, 28, 29, 30, 53, 2, 70, 55] -- (53,55)

x = 54

-----------------------------------------------

diff z t = abs (z - t)

myFloor _ [] z = z
myFloor n (x : xs) z
  | (x > z) && (diff x n < diff z n) = myFloor n xs x
  | otherwise = myFloor n xs z

myCeil _ [] z = z
myCeil n (x : xs) z
  | (x < z) && (diff x n < diff z n) = myCeil n xs x
  | otherwise = myCeil n xs z

-- Exemplo de Teste: <gchi> findFloorAndCeil x arr
-- (-2147483647) e 2147483647 representam -infinito e +infinito
findFloorAndCeil x array = (myFloor x (filter (<= x) array) (-2147483647), myCeil x (filter (>= x) array) 2147483647)
