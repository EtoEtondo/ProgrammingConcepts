-- take 10 primes

primes :: Integral a => [a] --Num ging nicht nande?
primes = [x | x <- [1..], isPrime x] --bsp bis 10

isPrime :: Integral a => a -> Bool
isPrime x
    | x == 1 = False
    | x == 2 = True
    | (length [n | n <- [2..x-1], mod x n == 0]) > 0 = False --liste 3 bis 9, diew werte mod dem aktuellen x Wert, checken ob teilbar, wenn ja dann in liste -> keine PZ, länge bestimmt Anzahl der Teiler außer der aktuelle Wert selbst
    | otherwise = True

primeFactors :: Integral a => a -> [a]
primeFactors n = factorize n primes
    where
        factorize n (x:xs)
            | x * x > n = [n] --wenn keine Zerlegung möglich / Abbruchbedingung
            | mod n x == 0 = x : factorize (div n x) (x:xs) --Zerlegung
            | otherwise = factorize n xs --mit nächster PZ prüfen