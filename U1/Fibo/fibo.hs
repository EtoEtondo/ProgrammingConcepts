fibonacci :: Num a => [a]
fibonacci = [0] ++ [1] ++ adding (+) fibonacci (tail fibonacci)
    where adding f (x:xs) (y:ys) = [f x y] ++ adding f xs ys
