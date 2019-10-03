findC :: Char -> [(Char, Char)] -> Char
findC ' ' [] = ' '
findC c ((a,b):tail) | c == a = b
                     | otherwise = findC c tail

decEnigma :: String -> [(Char, Char)] -> String
decEnigma [] x = []
decEnigma (a:as) x = (findC a x):(decEnigma as x)