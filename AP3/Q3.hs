data Comando = ParaFrente Int
      | ParaTras Int
      | Escreva Char
      deriving (Show, Eq)

sub :: Int -> Char -> String -> String
sub _ _ [] = []
sub x a (y:ys) | x==0 = a:sub (x-1) a ys
               | otherwise = y:sub (x-1) a ys

controla :: Int -> String -> [Comando] -> Char
controla x a [ParaFrente y] = a!!(x+y)
controla x a [ParaTras y] = a!!(x-y)
controla x a [Escreva y] = y
controla x a ((ParaFrente y):xs) = controla (x+y) a xs
controla x a ((ParaTras y):xs) = controla (x-y) a xs
controla x a ((Escreva y):xs) = controla x (sub x y a) xs

interprete :: String -> [Comando] -> Char
interprete a [] = ' '
interprete a b = controla 0 a b