reqs  = client initMsg resps
resps = server reqs

client init ~(resp:resps) = init : client (next resp) resps
server       (req:reqs)   = process req : server reqs

initMsg = 0
next resp = resp
process req = req + 1

-- implicit lazy pattern
fib@(1:tfib) = 1 : 1 : [a+b | (a,b) <- zip fib tfib]

-- type signature in let expression
foo = let foo' :: Integer -> Integer
          foo' x = x + 5
      in foo' 9

-- implicit lazy pattern in let expression
bar = let fibs@(1:tfibs) = 1 : 1 : [a+b | (a,b) <- zip fibs tfibs]
      in take 12 tfibs
