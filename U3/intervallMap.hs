a = singleton 'a' :: IntervalMap Int Char
b = insert 10 20 'b' a
c = insert 9 21 'c' b
d = insert 5 15 'd' c
e = insert 14 22 'e' d
f = insert 10 19 'f' e
g = fmap fromEnum f
h = "Hello" <$ g
i = insert 5 10 110 $ insert 10 15 90 $ singleton 100 :: IntervalMap Int Int
j = insert 5 10 (-) $ insert 10 15 (*) $ singleton (+) :: IntervalMap Int (Int -> Int -> Int)
k = insert 3 18 2 $ singleton 10 :: IntervalMap Int Int
-- l = j <*> i <*> k

-- [(k, v)] ist nötig für weitere Zugriffe mit insert
-- nur v wäre nur der eine Value
data IntervalMap k v = IntervalMap v [(k, v)]
    deriving(Show)

-- functor, wenn mit leerer liste wird die auszuführende funktion nur auf default value ausgeführt
-- ansonsten wird funktion auf die einzelnen value-werte der keys gemappt und 
-- durch zip werden die key-value paare wieder zusammengeführt
instance Functor (IntervalMap k) where
    fmap f (IntervalMap v []) = IntervalMap (f v) []
    fmap f (IntervalMap v xs) = IntervalMap (f v) (zip (map fst xs) (map f $ map snd xs))

-- instance Ord k => Applicative (IntervalMap k) where
--     (<*>) (IntervalMap v x) (IntervalMap v2 x2) = IntervalMap (v v2) (... x x2 0)


-- wirklich nur für den ersten Wert
singleton :: v -> IntervalMap k v
singleton v = IntervalMap v []


(!) :: Ord k => IntervalMap k v -> k -> v
-- wenn nur default-Wert vorhanden ist
(!) (IntervalMap v []) _ = v
-- wenn der gewünschte Key kleiner als der Key des ersten Element in der Liste ist gib default-Wert
-- genauso, wenn der gewünschte Key größer als der Key des letzten Element in der Liste ist
-- wenn sich der Key innerhalb befindet, gib vom letzten gefundenen Element durch takeWhile
-- den Value zurück
(!) (IntervalMap v xs) key | key < (fst $ head xs) = v
                           | key > (fst $ last xs) = v 
                           | otherwise = snd $ last $ takeWhile (\(k, _) -> k <= key) xs

insert :: Ord k => k -> k -> v -> IntervalMap k v -> IntervalMap k v
-- In der Liste wird angegeben zwischen welchen Key Werten sich der neue Value befindet 
insert begin end val (IntervalMap v []) = IntervalMap v [(begin, val), (end, v)]
-- TODO alle fälle testen
insert begin end val (IntervalMap v xs) -- wenn nichts vor begin gibt und nichts nach end
                                        | length [xs2 | xs2<-xs, (\(k, v) -> k < begin) xs2] == 0 && length [xs2 | xs2<-xs, (\(k, v) -> k > end) xs2] == 0 = IntervalMap v ([(begin, val)] ++ [(end, v)]) 
                                        -- wenn nichts vor beginn aber nach end                                     anfangswert   ++ end mit letztem wert vor endwert                                ++ alles nach end
                                        | length [xs2 | xs2<-xs, (\(k, v) -> k < begin) xs2] == 0 = IntervalMap v ([(begin, val)] ++ [(end, snd $ last $ [xs2 | xs2<-xs, (\(k, v) -> k <= end) xs2])] ++ [xs2 | xs2<-xs, (\(k, v) -> k > end) xs2])
                                        -- wenn nichts nach end aber vor beginn                                     alles vor beginn                         ++  anfangswert   ++ endwert 
                                        | length [xs2 | xs2<-xs, (\(k, v) -> k > end) xs2] == 0 = IntervalMap v ([xs2 | xs2<-xs, (\(k, v) -> k < begin) xs2] ++ [(begin, val)] ++ [(end, v)])
                                        -- ansonsten                    alles vor beginn                         ++ anfangswert    ++  end mit letztem wert vor endwert                                ++ alles nach end
                                        | otherwise = IntervalMap v ([xs2 | xs2<-xs, (\(k, v) -> k < begin) xs2] ++ [(begin, val)] ++ [(end, snd $ last $ [xs2 | xs2<-xs, (\(k, v) -> k <= end) xs2])] ++ [xs2 | xs2<-xs, (\(k, v) -> k > end) xs2])
