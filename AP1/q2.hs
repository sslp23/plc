len :: String -> Int  
len [] = 0  
len (x:xs) = 1 + len xs

ctoi :: Char -> Int
ctoi x | x=='0' = 0
       | otherwise = 1

btoi :: String -> Int
btoi [] = 0
btoi (a:as) = (ctoi a)*(2^len as) + btoi as