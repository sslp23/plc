
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

nextMes :: String -> String
nextMes [] = []
nextMes (x:xs) | x <= '9' && head xs == ';' = tail xs
               | otherwise = nextMes xs

getMin :: String -> Double
getMin (x:xs) | nextMes xs == [] = getGasto xs
              | getGasto xs <= getMin(nextMes xs) = getGasto xs
              | otherwise = getMin (nextMes xs)

getMax :: String -> Double
getMax (x:xs) | nextMes xs == [] = getGasto xs
              | getGasto xs >= getMax(nextMes xs) = getGasto xs
              | otherwise = getMax (nextMes xs)

minMaxCartao :: String -> (Double, Double)
minMaxCartao x = (getMin x, getMax x)