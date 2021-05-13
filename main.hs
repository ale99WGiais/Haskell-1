module Esercizi where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

combinazioni :: Integer -> Integer -> Integer
combinazioni n k = div (factorial n)  ((factorial k) * (factorial (n-k)))

combinazioniElementi :: (Eq a) => [a] -> [[a]]
combinazioniElementi [] = [[]]
combinazioniElementi xs = foldr (\x acc -> x ++ acc) [] (map (\x -> (map (\y -> x:y) (combinazioniElementi [t | t <- xs, t /= x]))) xs)

rimuoviPosPari :: [a] -> [a]
rimuoviPosPari [] = []
rimuoviPosPari [x] = [x]
rimuoviPosPari (x:_:xs) = x:(rimuoviPosPari xs)

sommaDispari :: (Num a) => [a] -> a
sommaDispari = sum . rimuoviPosPari

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

coppieConSommeSuffisse :: Num b => [b] -> [(b, b)]
coppieConSommeSuffisse xs = zip xs (scanr1(+) xs)

coppieConSommePrefisse :: Num b => [b] -> [(b, b)]
coppieConSommePrefisse xs = zip xs (scanl1(+) xs)

shiftToZero :: (Num a, Ord a) => [a] -> [a]
shiftToZero xs = res
  where
      shiftToZeroHelp (mi, []) = (mi, [])
      shiftToZeroHelp (mi, x:xs) = (newMi, (x-newMi):newXs)
          where
          (newMi, newXs) = shiftToZeroHelp ((min mi x), xs)
      (_, res) = shiftToZeroHelp (head xs, xs)



matrix_dim :: [[a]] -> (Int, Int)
matrix_dim mat
  | c == -1 = (-1, -1)
  | otherwise = (r, c)
  where
    row_dims :: [[a]] -> [Int]
    row_dims = map length
    row_dim xs
      | allSame = (head dims)
      | otherwise = -1
      where
        dims = row_dims xs
        allSame = all (== (head dims)) dims
    r = length mat
    c = row_dim mat


