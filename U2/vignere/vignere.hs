key = "hochschule"
text = "Haskell ist gar nicht so schwer!"
text2 = "Iemdl szkzl ihlym dlnlv zqjdw t!"
zitat = "zitat"
mail = "Vqkkh dvgeg mckeb mmduk ymWil sigzb mlbeS tsnny sjeiv jmg,t amkdh qbdox mvxnp hzxig dUxnz daxhx m,pal fmmag vmkdx munsl .-Tlt mBnrb mo"


encryptVigenere :: [Char] -> [Char] -> [Char]
encryptVigenere [] [] = []
encryptVigenere k n = space (map toEnum (map vig (zip (map fromEnum (cycle k)) (filter (/=32) (map fromEnum n)))))
    where vig (x,y) | y < (fromEnum 'A') = y
                    | y > (fromEnum 'z') = y
                    | y > (fromEnum 'Z') && y < (fromEnum 'a') = y
                    | y >= (fromEnum 'a') && x >= (fromEnum 'a') = (x - (fromEnum 'a') + y - (fromEnum 'a') + 26) `mod` 26 + (fromEnum 'a')
                    | y >= (fromEnum 'A') && x >= (fromEnum 'a') = (x - (fromEnum 'a') + y - (fromEnum 'A') + 26) `mod` 26 + (fromEnum 'A')
                    | y >= (fromEnum 'a') && x >= (fromEnum 'A') = (x - (fromEnum 'A') + y - (fromEnum 'a') + 26) `mod` 26 + (fromEnum 'a')
                    | y >= (fromEnum 'A') && x >= (fromEnum 'A') = (x - (fromEnum 'A') + y - (fromEnum 'A') + 26) `mod` 26 + (fromEnum 'A')
          space y   | length y <= 5 = y
                    | otherwise = take 5 y ++ " " ++ space (drop 5 y)


decryptVigenere :: [Char] -> [Char] -> [Char]
decryptVigenere x y = map toEnum (map vig (zip (cycle (map fromEnum x)) (map fromEnum (filter (/=' ') y))))
    where vig (x, y) | y < (fromEnum 'A') = y
                     | y > (fromEnum 'z') = y
                     | y > (fromEnum 'Z') && y < (fromEnum 'a') = y
                     | y >= (fromEnum 'a') && x >= (fromEnum 'a') = (y - (fromEnum 'a') - (x - (fromEnum 'a')) + 26) `mod` 26 + (fromEnum 'a')
                     | y >= (fromEnum 'A') && x >= (fromEnum 'a') = (y - (fromEnum 'A') - (x - (fromEnum 'a')) + 26) `mod` 26 + (fromEnum 'A')
                     | y >= (fromEnum 'a') && x >= (fromEnum 'A') = (y - (fromEnum 'a') - (x - (fromEnum 'A')) + 26) `mod` 26 + (fromEnum 'a')
                     | y >= (fromEnum 'A') && x >= (fromEnum 'A') = (y - (fromEnum 'A') - (x - (fromEnum 'A')) + 26) `mod` 26 + (fromEnum 'A')














