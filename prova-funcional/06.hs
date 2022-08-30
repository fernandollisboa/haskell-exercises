-- 6. Implementar uma pilha e seus algoritmos em Haskell
data Stack a = Stack [a] deriving (Show)

myPush a (Stack xs) = Stack (a : xs)

myPop (Stack []) = error "Stack vazia"
myPop (Stack [x]) = x
myPop (Stack (x : xs)) = x

myUnstack (Stack []) = error "Stack vazia"
myUnstack (Stack [x]) = Stack [x]
myUnstack (Stack (x : xs)) = Stack xs