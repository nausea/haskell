data (Ord a) => Tree a = Leaf a | Branch (Tree a) (Tree a)

fringe :: (Ord a) => Tree a -> [a]
fringe (Leaf v)     = [v]
fringe (Branch l r) = fringe l ++ fringe r

exTree = Branch (Branch (Leaf 5) (Branch (Leaf 3) (Leaf 7))) (Leaf 10)

-- data Color = Red | Green | Blue | Brown | Purple | Yellow
-- Following equations has a type error, cause Color is not in the Ord class
--  and cannot be used with Tree
-- colorTree = Branch (Leaf Red) (Leaf Blue)


instance Functor Tree where
    fmap f (Leaf x)     = Leaf (f x)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
