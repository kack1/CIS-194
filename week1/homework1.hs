-- Credit card validator 
-- Use the "validate" function to validate card number
toDigits :: Integer -> [Integer]
toDigits x 
    | x <= 0    = []
    |otherwise  = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0    = []
    |otherwise  = x `mod` 10 : toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther n
    | even (length n)   = 2 * head n : doubleEveryOther (tail n)
    | otherwise         = head n : doubleEveryOther (tail n)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits $ n) `mod` 10 == 0


--Tower of Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n fromRod toRod auxRod
    | n == 0 = []
    | otherwise = hanoi (n-1) fromRod auxRod toRod ++ topDisk ++ hanoi (n-1) auxRod toRod fromRod
    where topDisk = [(fromRod, toRod)]
