type Comando = String
type Valor = Int

sum' :: [Int] -> Int  
sum' [] = 0  
sum' (x:xs) = x + sum' xs

executa :: [(Comando, Valor)] -> Int
executa x = next 0 x 

next :: Int -> [(Comando, Valor)] -> Int
next x [] = 0
next x [(a,b)] | a == "Divide" && b == 0 = -666
               | a == "Divide" = quot x b
               | a == "Multiplica" = x*b
               | a == "Soma" =  x+b
               | a == "Subtrai" =  x-b
next x ((a,b):as) | a == "Divide" && b == 0 = -666
                  | a == "Divide" = next (quot  x b) as
                  | a == "Multiplica" = next (x*b) as
                  | a == "Soma" =  next (x+b) as
                  | a == "Subtrai" =  next (x-b) as