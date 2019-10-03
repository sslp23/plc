len :: [Double] -> Int  
len [] = 0  
len (x:xs) = 1 + len xs

credito :: Int -> Double -> Int -> [Double] -> [Double]
credito a b c [] = []
credito l valor contaO x | (l - (len x)) == contaO = (head x+valor):credito l valor contaO (tail x)
                         | otherwise = (head x):credito l valor contaO (tail x)

debito :: Int -> Double -> Int -> [Double] -> [Double]
debito a b c [] = []
debito l valor contaO x | ((l - (len x)) == contaO && ((head x-valor) >= 0)) = (head x-valor):debito l valor contaO (tail x)
                        | otherwise = (head x):debito l valor contaO (tail x)

transf :: Int -> Double -> Int -> Int -> [Double] -> [Double]
transf a b c d [] = []
transf l valor contaO contaD x | (l - (len x)) == contaO && ((head x-valor) >= 0) = (head x-valor):transf l valor contaO contaD (tail x)
                               | (l - (len x)) == contaD = (head x+valor):transf l valor contaO contaD (tail x)
                               | otherwise = (head x):transf l valor contaO contaD (tail x)

processBankOperations :: [Double] -> [(Int, Int, Int, Double)] -> [Double]
processBankOperations x [] = x
processBankOperations x ((a,b,c,d):as) | a == 0 = processBankOperations (credito (len x) d b x) as
                                       | a == 1 = processBankOperations (debito (len x) d b x) as
                                       | a == 2 = processBankOperations (transf (len x) d b c x) as