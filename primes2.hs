primes :: [Integer]
primes = sieve [2..]
    where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main = putStrLn $ show $ take 10000 primes 
