fpos :: [Int] -> [Int] -> Int
fpos as bs = length . filter (\x -> x > 0) $ zipWith (-) as bs
fneg :: [Int] -> [Int] -> Int
fneg as bs = length . filter (\x -> x < 0) $ zipWith (-) as bs

solve :: [Int] -> [Int]
solve xs = [fpos a b, fneg a b]
    where (a,b) = splitAt ((length xs  + 1) `div` 2 ) xs

main = interact $  unwords . map show . solve . map read . words
