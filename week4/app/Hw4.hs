module Hw4 where

import qualified Data.List
import qualified Data.Bits

fun1' :: [Integer] -> Integer
fun1' =  sum . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)


fun2' :: Integer -> Integer
fun2' = sum 
    . filter even 
    . takeWhile (>1) 
    . iterate (\x -> if even x then x `div` 2 else 3 * x +1)

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

height :: Tree a -> Integer
height (Node h _ _ r) = h
height Leaf = 0

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h Leaf y Leaf) = Node 1 (insert x Leaf) y Leaf
insert x (Node h Leaf y right) = Node h (insert x Leaf) y right
insert x (Node h left y Leaf) = Node h left y (insert x Leaf)
insert x (Node h left y right) =
    let (leftH, rightH) = (height left, height right) in
    case compare leftH rightH of
        LT -> Node h (insert x left) y right
        GT -> Node h left y (insert x right)
        EQ -> Node (1 + height right') left y right'
    where right' = insert x right

foldTree :: [a] -> Tree a
foldTree = Data.List.foldr insert Leaf


xor :: [Bool] -> Bool
xor = foldl Data.Bits.xor False
