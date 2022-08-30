-- 5. Implementar a estrutura de dados Heap e seus algoritmos (usando datatype) em Haskell
data Heap a = NIL | Node a (Heap a) (Heap a) deriving (Eq, Show, Ord)

myInsert x NIL = Node x NIL NIL
myInsert x (Node a left right)
  | x > a = Node a left (myInsert x right)
  | x < a = Node a (myInsert x left) right
  | otherwise = Node x left right

mySearch x NIL = Node x NIL NIL
mySearch x (Node a left right)
  | x > a = mySearch x right
  | x < a = mySearch x left
  | otherwise = Node x left right

myMax NIL = error "Heap vazia"
myMax (Node a _ NIL) = a
myMax (Node a left right) = myMax right

myMin NIL = error "Heap vazia"
myMin (Node a NIL _) = a
myMin (Node a left right) = myMin left

mySizeHeap NIL = 0
mySizeHeap (Node a left right) = 1 + mySizeHeap left + mySizeHeap right

inOrder NIL = []
inOrder (Node a left right) = inOrder left ++ [a] ++ inOrder right

preOrder NIL = []
preOrder (Node a left right) = [a] ++ preOrder left ++ preOrder right

postOrder NIL = []
postOrder (Node a left right) = postOrder left ++ postOrder right ++ [a]
