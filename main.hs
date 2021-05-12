module Esercizi where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

combinazioni :: Integer -> Integer -> Integer
combinazioni n k = div (factorial n)  ((factorial k) * (factorial (n-k)))

combinazioniElementi :: (Eq a) => [a] -> [[a]]
combinazioniElementi [] = [[]]
combinazioniElementi xs = foldr (\x acc -> x ++ acc) [] (map (\x -> (map (\y -> x:y) (combinazioniElementi [t | t <- xs, t /= x]))) xs)

rimuoviPari :: [a] -> [a]
rimuoviPari [] = []
rimuoviPari [x] = [x]
rimuoviPari (x:_:xs) = x:(rimuoviPari xs)

sommaDispari :: (Num a) => [a] -> a
sommaDispari = sum . rimuoviPari

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort $ filter (<=x) xs) ++ [x] ++ (quickSort $ filter (>x) xs)

dueMinori :: Ord a => [a] -> (Maybe a, Maybe a)
dueMinori [] = (Nothing, Nothing)
dueMinori (x:[]) = (Just x, Nothing)
dueMinori xs = (Just a, Just b) where (a:b:_) = (quickSort xs)

dispari :: [Integer] -> [Integer]
dispari = filter (\x -> mod x 2 == 1)

dueMinoriDispari :: [Integer] -> (Maybe Integer, Maybe Integer)
dueMinoriDispari = dueMinori . dispari