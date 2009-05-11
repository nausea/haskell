fibs = 1 : 1 : [(a+b) | (a, b) <- zip fibs (tail fibs)]
