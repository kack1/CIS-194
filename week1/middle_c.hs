getMiddle :: String -> String
getMiddle s 
    | odd n = [s !!  halfN]
    | otherwise = [s !! (halfN - 1), s !! halfN]
    where n = length s
          halfN = length s `div` 2
