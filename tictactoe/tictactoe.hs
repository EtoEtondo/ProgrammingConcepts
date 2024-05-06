import System.Exit
import System.Random

-- Datentyp für Züge
data State = X | O | Empty deriving (Eq, Read)

instance Show State where
    show X = "  X "
    show O = "  O "
    show Empty = "    "

-- instanziieren des Spielfelds
type Field = [State] -- 0 bis 8
field = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]

-- Trennlinie
line :: String
line = "+---+---+---+\n"

-- Ausgabe des Spielfelds
printField :: Field -> IO()
printField field = do
    putStrLn ("\n" ++ (show (field !! 0)) ++ (show (field !! 1)) ++ (show (field !! 2)) ++ "\n" ++ line ++ (show (field !! 3)) ++ (show (field !! 4)) ++ (show (field !! 5)) ++ "\n" ++ line ++ (show (field !! 6)) ++ (show (field !! 7)) ++ (show (field !! 8)) ++ "\n")

-- Überprüfung der Eingabe
nextMove :: Int -> Field -> State -> Int -> IO()
nextMove move list state bot = do
	if (move < 0) || (move > 8)
		then do
			if (bot == 0)
				then do
					putStrLn "Invalid input"
					playGame list state bot
				else do
					playGame list state bot
		else do
			if (checkFree list move)
				then do
					let newfield = setMove move state list
					if (checkWin state newfield)
						then do
							putStrLn (">>" ++ (show state) ++ " wins! <<")
							printField newfield
							menu
						else if (checkFullField newfield)
							then do
								putStrLn ">> Equal Game - Full gameboard <<"
								printField newfield
								menu
						else do
							let newstate = changeState state
							startGame newfield newstate bot
				else do
					if (bot == 0)
						then do
							putStrLn "Field is not empty"
							playGame list state bot
						else do
							playGame list state bot

-- ausführen des Spiels
playGame :: Field -> State -> Int -> IO()
playGame list state bot = do
	if (bot == 1) && (state == O)
		then do
			move <- randomRIO(0,8)
			nextMove move list state bot
		else do		
			putStrLn "Enter your Move 0-8: " 
			choice <- getLine
			let move = (read choice :: Int)
			nextMove move list state bot

-- überprüfen ob Spielfeld schon voll
checkFullField :: Field -> Bool 
checkFullField list | Empty `notElem` list = True
                    | otherwise = False

-- überprüfen ob gewonnen wurde
checkWin :: State -> Field -> Bool
checkWin state list | ((list !! 0) == state) && ((list !! 1) == state) && ((list !! 2) == state) = True
                    | ((list !! 3) == state) && ((list !! 4) == state) && ((list !! 5) == state) = True
					| ((list !! 6) == state) && ((list !! 7) == state) && ((list !! 8) == state) = True
					| ((list !! 0) == state) && ((list !! 3) == state) && ((list !! 6) == state) = True
					| ((list !! 1) == state) && ((list !! 4) == state) && ((list !! 7) == state) = True
					| ((list !! 2) == state) && ((list !! 5) == state) && ((list !! 8) == state) = True
					| ((list !! 0) == state) && ((list !! 4) == state) && ((list !! 8) == state) = True
					| ((list !! 2) == state) && ((list !! 4) == state) && ((list !! 6) == state) = True
					| otherwise = False

-- einfügen des Zuges
setMove :: Int -> a -> [a] -> [a]
setMove i add list = (take i list) ++ add : (drop (i+1) list)

-- überprüfen ob Feld frei
checkFree :: Field -> Int -> Bool
checkFree list move | (list !! move) == Empty = True
                    | otherwise = False

-- Spieler wechseln
changeState :: State -> State
changeState state | X == state = O
                  | O == state = X

-- startet das Spiel
startGame :: Field -> State -> Int -> IO()
startGame field currentPlayer bot = do
	printField field
	playGame field currentPlayer bot

-- Spiel starten oder direkt beenden
selectChoice :: String -> IO ()
selectChoice choice | choice == "q" = exitSuccess
					| choice == "b" = startGame field X 1
                    | otherwise = startGame field X 0

-- Hauptmenu
menu :: IO()
menu = do
    putStrLn "\n-------------------------------------------"
    putStrLn "Tic Tac Toe Game"
    putStrLn ">> Press enter to start PvP << \n>> Press b to start PvE <<"
    putStrLn ">> Press q to quit <<"
    putStrLn "-------------------------------------------\n"
    getLine >>= selectChoice
    menu

main = do
    menu