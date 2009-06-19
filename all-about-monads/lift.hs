import Data.List (foldl')
import Monad (liftM2)

allCombinations :: (a -> a -> a) -> [[a]] -> [a]
allCombinations _  []     = []
allCombinations fn (l:ls) = foldl' (liftM2 fn) l ls
