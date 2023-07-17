data IntList = Empty | Cons Int IntList
  deriving Show

absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

addOne x = x + 1
square x = x * x

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList f Empty         = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)


data List t = E | C t (List t)

filterList _ E = E
filterList p (C x xs)
    | p x       = C x (filterList p xs)
    | otherwise = filterList p xs


mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)
