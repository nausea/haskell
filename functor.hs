{-# LANGUAGE FlexibleInstances #-}

instance Functor ((->) Integer) where
    fmap g f = \x -> g $ f 0
