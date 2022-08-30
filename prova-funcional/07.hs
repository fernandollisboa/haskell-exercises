data Queue a = Queue [a] deriving (Show)

myEnqueue a (Queue q) = Queue (q ++ [a])

myNext (Queue []) = error "Queue Vazia"
myNext (Queue (q : qs)) = q

myDequeue (Queue []) = error "Queue vazia"
myDequeue (Queue (q : qs)) = Queue qs
