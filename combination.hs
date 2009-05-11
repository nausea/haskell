combinations []     = [[]]
combinations (x:xs) = combinations xs ++ [(x:xs')| xs' <- combinations xs]

-- Tail recursive version:
comb xs = comb' xs [[]]
    where comb' []     acc = acc
          comb' (x:xs) acc = comb' xs (acc ++ map (x:) acc)
