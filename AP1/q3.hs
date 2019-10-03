len :: [Int] -> Int  
len [] = 0  
len (x:xs) = 1 + len xs

metade :: [Int] -> ([Int], [Int])
metade x = (take (quot (len x) 2) x, drop (quot (len x) 2) x)