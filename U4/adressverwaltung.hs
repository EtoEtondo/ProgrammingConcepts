import Control.Monad (when)
import Data.Strings

data Adress = Adress
    { vorname :: String
    , nachname :: String
    , strasse :: String
    , plz  :: String
    , stadt :: String
    , tel :: String
    }

instance Show Adress where
    show (Adress vorname nachname strasse plz stadt tel) = "[" ++ vorname ++ ", " ++ nachname ++ ", " ++ strasse ++ ", " ++ plz ++ ", " ++ stadt ++ ", " ++ tel ++ "]"

addKontakt :: IO ()
addKontakt = do 
    vorname <- putStr "Geben Sie den Vornamen ein: " >> getLine
    nachname <- putStr "Geben Sie den Nachnamen ein: " >> getLine
    strasse <- putStr "Geben Sie die Strasse mit Hausnummer ein: " >> getLine
    plz <- putStr "Geben Sie die Postleitzahl ein: " >> getLine
    stadt <- putStr "Geben Sie die Stadt ein: " >> getLine
    tel <- putStr "Geben Sie die Telefonummer ein: " >> getLine
    let newKontakt = Adress vorname nachname strasse plz stadt tel
    appendFile "adressen.txt" (show newKontakt ++ "\n")
    

deleteKontakt :: IO ()
deleteKontakt = do
    showKontakte
    allKontakte <- readFile "adressen.txt"
    let allKontakteLines = lines allKontakte 
    line <- putStr "Geben Sie die Nummer des Kontaktes an, den Sie entfernen wollen: " >> getLine
    when ((length allKontakteLines) > 0) $
        writeFile "adressen.txt" (deleteLine 0 line allKontakteLines)
    

deleteLine :: Int -> String -> [String] -> String
deleteLine line delline [] = ""
deleteLine line delline x | show(line) == delline = "" ++ (deleteLine (line+1) delline (tail x))
                          | otherwise = (head x) ++ "\n" ++ (deleteLine (line+1) delline (tail x))

editKontakt :: IO ()
editKontakt = do
    showKontakte
    allKontakte <- readFile "adressen.txt"
    let allKontakteLines = lines allKontakte 
    line <- putStr "Geben Sie die Nummer des Kontaktes an, den Sie bearbeiten wollen: " >> getLine
    vorname <- putStr "Geben Sie den Vornamen ein: " >> getLine
    nachname <- putStr "Geben Sie den Nachnamen ein: " >> getLine
    strasse <- putStr "Geben Sie die Strasse mit Hausnummer ein: " >> getLine
    plz <- putStr "Geben Sie die Postleitzahl ein: " >> getLine
    stadt <- putStr "Geben Sie die Stadt ein: " >> getLine
    tel <- putStr "Geben Sie die Telefonummer ein: " >> getLine
    let newKontakt = Adress vorname nachname strasse plz stadt tel
    when ((length allKontakteLines) > 0) $
        writeFile "adressen.txt" (editKontaktOnLine 0 line allKontakteLines newKontakt)

editKontaktOnLine :: Int -> String -> [String] -> Adress -> String
editKontaktOnLine line delline [] _ = ""
editKontaktOnLine line editline x newKontakt | show(line) == editline = show(newKontakt) ++ "\n" ++ (editKontaktOnLine (line+1) editline (tail x) newKontakt)
                                             | otherwise = (head x) ++ "\n" ++ (editKontaktOnLine (line+1) editline (tail x) newKontakt)

showKontakte :: IO ()
showKontakte = do
    allKontakte <- readFile "adressen.txt" 
    let allKontakteLines = lines allKontakte 
    putStrLn(printKontakteWithLinenumbers 0 allKontakteLines)
    
printKontakteWithLinenumbers :: Int -> [String] -> String
printKontakteWithLinenumbers line [] = ""
printKontakteWithLinenumbers line x = (show line) ++ ": " ++ (head x) ++ "\n" ++ (printKontakteWithLinenumbers (line+1) (tail x))

searchByVorname :: IO ()
searchByVorname = do
    allKontakte <- readFile "adressen.txt" 
    let allKontakteLines = lines allKontakte
    vorname <- putStrLn "Geben Sie den Vornamen des Kontaktes an, den Sie finden wollen: " >> getLine
    when ((length allKontakteLines) > 0) $
        putStrLn (searchName 0 vorname allKontakteLines)

searchByNachname :: IO ()
searchByNachname = do
    allKontakte <- readFile "adressen.txt" 
    let allKontakteLines = lines allKontakte
    nachname <- putStrLn "Geben Sie den Nachnamen des Kontaktes an, den Sie finden wollen: " >> getLine
    when ((length allKontakteLines) > 0) $
        putStrLn (searchName 1 nachname allKontakteLines)

searchName :: Int -> String -> [String] -> String
searchName modus vorname x | vorname == (splitStrings(head x) !! modus) = head x
                           | otherwise = searchName modus vorname (tail x)

splitStrings :: String -> [String]
splitStrings kontakt = do
    let a = strReplace "[" "" kontakt 
    let b = strReplace "]" "" a
    strSplitAll ", " b

selectChoice :: String -> IO ()
selectChoice choice | choice == "0" = addKontakt
                    | choice == "1" = deleteKontakt
                    | choice == "2" = editKontakt
                    | choice == "3" = showKontakte
                    | choice == "4" = searchByVorname
                    | choice == "5" = searchByNachname
                    | choice == "\n" = putStrLn "getChar nimmt direkt das Enter (\\n) nach der Eingabe"
                    | otherwise = putStrLn "Eingabe nicht gültig"

menu :: IO()
menu = do 
    putStrLn "\n\n-------------------------------------------"
    putStrLn "Ich bin Siri, was kann ich für Sie tun?"
    putStrLn "0: Hinzufügen eines Kontakts"
    putStrLn "1: Löschen eines Kontakts"
    putStrLn "2: Editieren eines Kontakts"
    putStrLn "3: Anzeigen aller Kontakte"
    putStrLn "4: Suchen von Kontakten nach Vornamen"
    putStrLn "5: Suchen nach Kontakten nach Nachnamen"
    putStrLn "-------------------------------------------\n"
    putStr "Ihre Wahl: " >> getLine >>= selectChoice

    menu

main = do
    menu
    