data Tree t = Nilt
            | Node t (Tree t) (Tree t)
            deriving (Show, Ord, Eq)

isBST :: (Eq t, Ord t, Num t) => Tree t -> Bool
isBST (Node a Nilt Nilt) = True
isBST (Node x l r) | x<getV(r) && x>=getV(l) = isBST(r) && isBST(l) -- right>x>=left
                    | otherwise = False
                   
getV :: Tree t -> t 
getV (Node x l r) = x