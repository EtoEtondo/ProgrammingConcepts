fizzBuzz :: (Integral a, Show a) => a -> [String]
fizzBuzz x = [fizz n | n <- [1..x]] --Hilffunktionen definirbar
    where
        fizz x
           | mod x 15 == 0 = "FizzBuzz"
           | mod x 5 == 0 = "Buzz"
           | mod x 3 == 0 = "Fizz"
           | otherwise  = show x

main = do
    print(fizzBuzz 30)