distP :: (Double, Double) -> (Double, Double) -> Double
distP (x1, y1) (x2, y2) = sqrt((x2-x1)^2 + (y2-y1)^2) 

isEqTriangle :: [(Double, Double)] -> Bool
isEqTriangle [] = False
isEqTriangle (x:xs) | abs(distP x (head xs)-distP x (last xs))<=0.000001 && 
                      abs(distP (head xs) (last xs)-distP x (head xs)) <= 0.000001 && 
                      abs(distP (head xs) (last xs)-distP x (last xs)) <= 0.000001 = True
                    | otherwise = False