len :: String -> Int  
len [] = 0  
len (x:xs) = 1 + len xs

isReplica :: String -> Int -> Char -> Bool
isReplica [] i c = True
isReplica (a:as) i c | (i-1) == len as && c==a = isReplica as (i-1) c
                     | otherwise = False 