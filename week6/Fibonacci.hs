{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Excersice 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s (streamFromSeed f (f s))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
-- ruler = interleaveStreams pattern1 pattern2
ruler = ruler' 0

ruler' x = interleaveStreams (streamRepeat x) (ruler' (x + 1))

-- Thought this would work... it does but doesn't
-- interleaveStreams :: Stream a -> Stream a -> Stream a
-- interleaveStreams (Cons a b) (Cons c d) =
-- Cons a (Cons c (interleaveStreams b d))
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons y ys) zs = Cons y (interleaveStreams zs ys)

-- Excersice 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap (* (-1))
  (+) (Cons x y) (Cons w z) = Cons (x + w) (y + z)
  (*) (Cons x xs) b@(Cons y ys) = Cons (x * y) ((streamMap (* x) ys) + (xs * b))

instance Fractional (Stream Integer) where
  (/) a@(Cons x xs) b@(Cons y ys) = q
    where
      q = Cons (x `div` y) (streamMap (`div` y) (xs - q * ys))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - (x * x))

-- Excersice 7
data Matrix =
  Matrix Integer Integer Integer Integer
  deriving (Show)

instance Num Matrix where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
    (Matrix
       (a11 * a12 + a21 * b21)
       (a11 * b12 + a12 * b22)
       (a21 * b11 + a22 * b21)
       (a21 * b12 + a22 * b22))

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = getFirstCell (f ^ (n - 1))
  where
    getFirstCell (Matrix a _ _ _) = a
    f = Matrix 1 1 1 0
