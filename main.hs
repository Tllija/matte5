
-- This function calculates the amount of cars at A and C after n months.
-- It does so via two functions that describe the amount of cars rented
-- depending on the amount of cars available and the current month n

-- n = the amount of months
-- a = cars at A at n = 0
-- ar = cars rented given A and n (i.e. F(A(n), n))
-- c = cars at C at n = 0
-- cr = cars rented given A and n (i.e. F(C(n), n))
iter3 :: (Integral a, Fractional b) => a -> -- n 
     b -> (b -> a -> b) ->                  -- a and anRented
     b -> (b -> a -> b) ->                  -- c and cnRented 
    (b, b)                                  -- a and c
iter3 n a ar c cr 
    | n == 0 = (a, c)
    | otherwise = iter3 (n-1) an ar cn cr
    where
        aCarsRented = ar a n
        cCarsRented = cr c n
        an = a - aCarsRented * 0.75 + cCarsRented * 0.30
        cn = c - cCarsRented * 0.30 + aCarsRented * 0.75


-- Same as iter3, but this outpus a list so that the change can be viewed
iter3Acc :: (Integral a, Fractional b) => a -> -- n
     b -> (b -> a -> b) ->                  -- a and anRented
     b -> (b -> a -> b) ->                  -- c and cnRented
    [(b, b)]                                -- a and c
iter3Acc n a ar c cr 
    | n == 0 = [(a, c)]
    | otherwise = ((an, cn)):(iter3Acc (n-1) an ar cn cr)
    where
        aCarsRented = ar a n
        cCarsRented = cr c n
        an = a - aCarsRented * 0.75 + cCarsRented * 0.30
        cn = c - cCarsRented * 0.30 + aCarsRented * 0.75

{-
Expl. of maths:

    A(0) + C(0) = 105 

    F1(A(n),n) describes the amount of cars RENTED F1 given the month n and amount of cars A(n)
    F2(C(n),n) does the same, but for C(n)

    A(n) is the amount of cars at A at month n
    C(n) is the amount of cars at C at month n

    A(n+1) = A(n) - 0.75 * F1(A(n), n) + 0.30 * F2(C(n), n)
    C(n+1) = C(n) - 0.30 * F2(C(n), n) + 0.75 * F1(A(n), n)


Result:
    The amount of cars at n doesn not necessarily approach a steady point, for example:
        If the function that describes the amount of rented cars at A is 
            F(a, n) = n (mod 12) * 0.05
        the amount of cars will always fluctuate depending on the time of the year.

-}




-- This functions is if the amount of cars rented is proportional to the amount of cars at either spot.
-- The proportinality constant is ~~vvvvv
cRentPercentage :: (Fractional a) => a
cRentPercentage = 0.5
aRentPercentage :: (Fractional a) => a
aRentPercentage = 0.1

-- n is the number of months to be simulated
-- a is the amount of cars at a at n=0
-- b is the amount of cars at b at n=0
iter2 :: (Integral a, Fractional b) => a -> b -> b -> (b, b)
iter2 n a c 
    | n == 0 = (a, c)
    | otherwise = iter2 (n-1) an cn
    where 
        cRented = c * cRentPercentage -- Amount of cars rented this month from c
        aRented = a * aRentPercentage -- Amount of cars rented this month from A
        cn = c - cRented * 0.30 + aRented * 0.75 -- 70% of A:s are returned and 75% of C:s are also returned here
        an = a - aRented * 0.75 + cRented * 0.30 -- 25% of C:s are returned and 30% of A:s are also returned here

{-
Expl. of maths:

    A(0) + C(0) = 105
    The inital distribution of cars doesn't matter when n -> infinity, s
    since the amount of cars will approach as certain value.

    C(n+1) = C(n) - rentPercentageC * C(n) * 0.30 + rentPercentageA * A(n) * 0.75)
    Cars at C at month n

    A(n+1) = A(n) - rentPercentageA * A(n) * 0.75 + rentPercentageC * C(n) * 0.30)
    Cars at A at month n

Result:

    If rentPercentage changes the result at n->infintity
    I.E. the amount of cars at approach a different value depending on the rentPercentage
-}



