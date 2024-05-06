--liste = ['a'..'e'] -- x = a, xs = b..e

myhead :: [a] -> a -- gibt erste Elemente aus Liste zurück a
myhead (x:_) = x

mytail :: [a] -> [a] -- gibt alle Elemente außer Erstes zurück bcde
mytail [] = []
mytail [x] = [x]
mytail (_:xs) = xs

mylast :: [a] -> a -- gibt letztes Element zurück e
mylast x = myhead(myreverse x)

myinit :: [a] -> [a] -- gibt alle Elemente außer Letztes zurück abcd
myinit [] = []
myinit x = myreverse(mytail(myreverse x))

myreverse :: [a] -> [a] -- umgekehrte Reihenfolge edcba
myreverse [] = []
myreverse [x] = [x]
myreverse (x:xs) = myreverse xs ++ [x]

--main = do
    --print (mylist liste)