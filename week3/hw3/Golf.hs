module Golf where

import Data.List

skips :: [a] -> [[a]]
skips [] = []
skips t = map (`nth` t) [1..l]
    where
    nth n t = map snd . filter (\x -> fst x `mod` n == 0) $ zip [1..] t
    l = length t

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:y:zs)
        | x > y     = x : localMaxima zs
        | x < y     = localMaxima (y:zs)
localMaxima x = []


histogram :: [Integer] -> String
histogram =
    unlines      -- and print it as lines
    . reverse      -- flip it upside down
    . (['0'..'9']:) . (replicate 10 '-':)  -- add the header rows
    . takeWhile (any (=='*'))  -- take rows until we run out of stars
    . transpose    -- switch from horizontal rows of stars to vertical columns
    . map (  (++ repeat ' ')       -- extend with (infinite) spaces
            . flip replicate '*'    -- represent count as stars
            . (subtract 1)          -- drop the extra number we added
            . length                -- get the count
          )        -- for each group
    . group        -- group into 0s, 1s, 2s, etc.
    . sort         -- sort the input
    . ([0..9]++)   -- add one of each to ensure non-zero counts
