arr = [10, 22, 28, 29, 30, 40]

x = 54

--------------------------
diff (z, t) = abs (z - t) -- erro ta aqui

findPair x arr = findPair' x arr (maximum arr, minimum arr) 0 (length arr - 1)

findPair' _ [] (z, t) _ _ = (z, t)
findPair' y (x : xs) (z, t) left right
  | sumDiff < prevDiff = findPair' y xs (x, last xs)
  | diff (x, last xs) > diff (z, t) = findPair' y xs (z, t)
  | otherwise = findPair' y (x : init xs) (z, t)
  where
    sumDiff = abs ((xs !! left) + (xs !! right) - y)
    prevDiff = abs (z + t - y)