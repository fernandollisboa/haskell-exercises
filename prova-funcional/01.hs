--1. Given a sorted array and a number x, nd a pair in array whose sum is closest to x. Examples:
arr = [10, 22, 28, 29, 30, 40] -- (22,30)

x = 54

--------------------------
diff (z, t) = abs (z - t) -- erro ta aqui

-- EXEMPLO DE TESTE: <ghci> findPair x arr
findPair x arr = findPair' x arr (-2147483647, 2147483647) 0 (length arr - 1)

findPair' _ [] (z, t) _ _ = (z, t)
findPair' y (x : xs) (z, t) left right
  | left >= right = (z, t)
  | sumDiff < prevDiff = findPair' y (x : xs) (x, last xs) left right
  | (xs !! left) + (xs !! right) > y = findPair' y (x : xs) (z, t) (left + 1) right
  | otherwise = findPair' y (x : xs) (z, t) left (right -1)
  where
    sumDiff = abs ((xs !! left) + (xs !! right) - y)
    prevDiff = abs (z + t - y)