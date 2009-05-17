primes = 2 : [x | x <- [3..], all (\y -> mod x y /= 0) $
              takeWhile (\y -> y * y <= x) primes]

main = putStrLn $ show $ take 10000 primes 
