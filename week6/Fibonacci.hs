-- Fibonacci Number: 
-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- Exercise 2
-- Fib2 is definently an improvement,
-- but cant figure out fibs2
fibs2 :: [Integer]
fibs2 = fib2 0 1

-- Interesting solution, but can't make 
-- it into an infinite list :(
fibNoGo :: Int -> [Integer]
fibNoGo 0 = [0]
fibNoGo 1 = [0, 1]
fibNoGo 2 = [0, 1, 1]
fibNoGo n = previous ++ [next]
  where
    previous = fibNoGo (n - 1) :: [Integer]
    next = previous !! (n - 1) + previous !! (n - 2) :: Integer

fib2 :: Integer -> Integer -> [Integer]
fib2 a b = a : fib2 b (a + b)

-- Exercise 3
data Stream a =
  Cons a (Stream a)
