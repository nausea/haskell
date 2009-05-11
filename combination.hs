combinations []     = [[]]
combinations (x:xs) = combinations xs ++ [(x:xs')| xs' <- combinations xs]
