hailstone :: Integer -> Integer
hailstone n
    | even n = n `div` 2
    | otherwise      = 3*n + 1

intListLength :: [Integer] -> Integer
intListLength []    = 0
intListLength (x:xs) = 1 + intListLength xs

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = []
hailstoneSeq n = n : hailstoneSeq (hailstone n)


hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n ) - 1

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []          = []
sumEveryTwo (x : [])    = [x]
sumEveryTwo (x:y:zs)    = (x + y) : sumEveryTwo zs
