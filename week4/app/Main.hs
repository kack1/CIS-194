module Main where

import Hw4
import Test.QuickCheck

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs


prop_fun :: Integer -> Bool
prop_fun x = fun2 x == fun2' x

main :: IO ()
main = putStrLn "Hello, Haskell!"
