text = "Haskell ist gar nicht so schwer!"
text2 = "Exphb iifpq dxokf zeqpl pzetb o!"

encryptCaesar :: Int -> [Char] -> [Char]
encryptCaesar _ [] = []
encryptCaesar x y = space (map toEnum (map shift (filter (/=32) (map fromEnum y))))
    where shift y | y < (fromEnum 'A') = y
                  | y > (fromEnum 'z') = y
                  | y > (fromEnum 'Z') && y < (fromEnum 'a') = y
                  | y >= (fromEnum 'a' - 1) = (y - (fromEnum 'a') + x) `mod` 26 + (fromEnum 'a')
                  | y >= (fromEnum 'A' - 1) = (y - (fromEnum 'A') + x) `mod` 26 + (fromEnum 'A')
          space y | length y <= 5 = y
                  | otherwise = take 5 y ++ " " ++ space (drop 5 y)


decryptCaesar :: Int -> [Char] -> [Char]
decryptCaesar x y = map toEnum (filter (/=32) (map fromEnum (encryptCaesar (-x) y)))
