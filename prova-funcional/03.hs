-- Given a sorted array an element x to be
-- searched, nd posi on of x in the array.
-- Input: arr[] = {10, 20, 40, 45, 55}
-- x = 45
-- Output: Element found at index 3
-- Input: arr[] = {10, 15, 25, 45, 55}
-- x = 15
-- Output: Element found at index 1

arr = [10, 20, 40, 45, 55]

x = 45 -- retorna 3

arr2 = [10, 15, 25, 45, 55]

x2 = 15 -- retorna 1

--------------------------------------------------
myBinarySearch k xs left right
  | left > right = -1
  | xs !! middle < k = myBinarySearch k xs left (middle - 1)
  | xs !! middle > k = myBinarySearch k xs (middle - 1) right
  | otherwise = middle
  where
    middle = left + ((right - left) `div` 2)

-- Exemplo de teste: <gchi> expSearch x arr
expSearch x arr = expSearch' x arr 1 0

expSearch' k xs index lastIndex
  | index > length xs = -1
  | k > last searchArr = expSearch' k xs (2 * index) index
  | k < last searchArr = myBinarySearch k xs lastIndex index
  | otherwise = index
  where
    searchArr = take (index + 1) xs