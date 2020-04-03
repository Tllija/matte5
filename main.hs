
{-
n = iterations
a = amount at airport at time 0
c = amount at city center at time 0
-}
iter :: Int -> Double -> Double -> (Double, Double)
iter n a c 
    | n == 0 = (a, c)
    | otherwise = iter (n-1) an cn
    where 
        cn = c * 0.75 + a * 0.7
        an = c * 0.25 + a * 0.3