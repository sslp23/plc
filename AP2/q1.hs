len :: String -> Int  
len [] = 0  
len (x:xs) = 1 + len xs

getValor :: String -> String
getValor [] = []
getValor (x:xs) | len xs <= 1 = getValor xs
                | x == ';' = getValor [last xs]
                | otherwise = x : getValor xs

getGasto :: String -> Double
getGasto [] = 0
getGasto (x:xs) | x == ';' && head xs <= '9' = read(getValor xs)  
                | otherwise = getGasto xs

checkMes :: String -> String
checkMes [] = []
checkMes (x:xs) | x <= 'z' && x >= 'A' = x:checkMes xs
                | len xs <= 1 = checkMes xs
                | x == ';' = checkMes [last xs]
                | otherwise = checkMes xs

nextMes :: String -> String
nextMes [] = []
nextMes (x:xs) | x <= '9' && head xs == ';' = tail xs
               | otherwise = nextMes xs

logMes :: String -> String -> Double
logMes _ [] = 0
logMes a (x:xs) | checkMes xs == a = getGasto xs + logMes a (nextMes xs)
                | otherwise = logMes a (nextMes xs)