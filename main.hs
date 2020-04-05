
{-
n = iterations
a = amount at airport at time 0
c = amount at city center at time 0
-}
iter :: (Integral a, Fractional b) => a -> b -> b -> (b, b)
iter n a c 
    | n == 0 = (a, c)
    | otherwise = iter (n-1) an cn
    where 
        cn = c * 0.75 + a * 0.7
        an = c * 0.25 + a * 0.3

{-
Explanation:

    Airport = A
    City-Center = C

    A revices 30% of C cars every month, as well as it loses 75% of cars from the start of the month.
    C recvices 70% of C:s cars from the start of the month and loses 25% of its own start-of-the-month cars.

    A(n+1) = A(n) * 0.3 + C(n) * 0.25
    C(n+1) = A(n) * 0.7 + C(n) * 0.75
    A(0) + C(0) = 105

    This model assumes every car gets rented at both A and C each month.

Which gives that:

    A(n) ~ 27.3 as n->infinity for any A(0)
         where A(0) = 105 - C(0)
    
    In fact, for any A(0) and C(0), as n->infinity
    A(n)/(A(n)+C(n)) ~ 0.26
-}