data Tree a = Nil | Node a (Tree a) (Tree a)
            deriving (Show)

foo :: Int -> Tree Int
foo n = Node n (foo (n - 1)) (foo (n + 1))

t2 = foo 0
