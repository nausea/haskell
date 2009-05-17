primes = 2 : [x | x <- [3,5..], all (\y -> mod x y /= 0) $
              takeWhile (\y -> y * y <= x) primes]

main = putStrLn $ show $ take 500000 primes 
