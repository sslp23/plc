import Data.Char

len :: String -> Int  
len [] = 0  
len (x:xs) = 1 + len xs

isPalindromo :: String -> Bool
isPalindromo [] = True
isPalindromo a  | len a == 1 = True 
                | toLower(head a) == toLower(head(reverse a)) = isPalindromo (reverse(tail(reverse(tail a))))
                | otherwise = False

