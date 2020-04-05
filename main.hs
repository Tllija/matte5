
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


cRentPercentage :: (Fractional a) => a
cRentPercentage = 0.5
aRentPercentage :: (Fractional a) => a
aRentPercentage = 0.1

iter2 :: (Integral a, Fractional b) => a -> b -> b -> (b, b)
iter2 n a c 
    | n == 0 = (a, c)
    | otherwise = iter2 (n-1) an cn
    where 
        cRented = c * cRentPercentage
        aRented = a * aRentPercentage
        cn = c - cRented * 0.25 + aRented * 0.70
        an = a - aRented * 0.70 + cRented * 0.25

{-
Expl.:

    A(n+1) = C(n) - rentPercentageC * C(n) * 0.75 + rentPercentageA * A(n) * 0.7)
    A(n+1) = A(n) - rentPercentageA * (A(n) * 0.3 + rentPercentageC * C(n) * 0.25)

Result:

    If rentpercentage changes the result at n->infintity
-}

iter3 :: (Integral a, Fractional b) => a -> -- n
     b -> (b -> b -> b) ->                  -- a and anRented
     b -> (b -> b -> b) ->                  -- c and cnRented
    (b, b)                                  -- a and c
iter3 n a ar c cr 
    | n == 0 = (a, c)
    | otherwise = iter3 (n-1) an ar cn cr
    where
        aCarsRented = ar a c
        cCarsRented = cr a c
        an = a - aCarsRented * 0.70 + cCarsRented * 0.25
        cn = c - cCarsRented * 0.25 + aCarsRented * 0.70


iter3Acc :: (Integral a, Fractional b) => a -> -- n
     b -> (b -> b -> b) ->                  -- a and anRented
     b -> (b -> b -> b) ->                  -- c and cnRented
    [(b, b)]                                -- a and c
iter3Acc n a ar c cr 
    | n == 0 = [(a, c)]
    | otherwise = ((an, cn)):(iter3Acc (n-1) an ar cn cr)
    where
        aCarsRented = ar a c
        cCarsRented = cr a c
        an = a - aCarsRented * 0.70 + cCarsRented * 0.25
        cn = c - cCarsRented * 0.25 + aCarsRented * 0.70

{-
Expl.:

    A(n+1) = A(n) - A(n) * 0.70 * F1(A(n), C(n)) + C(n) * 0.25 * F2(A(n), C(n))
    C(n+1) = C(n) - C(n) * 0.25 * F2(A(n), C(n)) + A(n) * 0.7 * F1(A(n), C(n))

-}