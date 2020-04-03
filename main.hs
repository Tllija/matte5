main = do 
    let a0 = 50
    let c0 = 65
    putStrLn $ show $ iter 10 50 50

iter :: Int -> Double -> Double -> (Double, Double)
iter n a c 
    | n == 0 = (a, c)
    | otherwise = iter (n-1) an cn
    where 
        cn = c * 0.75 + a * 0.7
        an = c * 0.25 + a * 0.3