newtype Natural = MakeNatural Integer
    deriving (Show,Eq)

toNatural :: Integer -> Natural
toNatural x | x < 0     = error "Can't create negative natural"
            | otherwise = MakeNatural x

fromNatural :: Natural -> Integer
fromNatural (MakeNatural i) = i

instance Num Natural where
    fromInteger = toNatural
    x + y = toNatural (fromNatural x + fromNatural y)
    x - y = let r = fromNatural x - fromNatural y
            in if r < 0 then error "Unnatural substraction"
                        else toNatural r
    x * y = toNatural (fromNatural x * fromNatural y)

