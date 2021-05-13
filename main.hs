module Esercizi where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

combinazioni :: Integer -> Integer -> Integer
combinazioni n k = div (factorial n)  ((factorial k) * (factorial (n-k)))

combinazioniElementi :: (Eq a) => [a] -> [[a]]
combinazioniElementi [] = [[]]
combinazioniElementi xs = foldr (\x acc -> x ++ acc) [] (map (\x -> (map (\y -> x:y) (combinazioniElementi [t | t <- xs, t /= x]))) xs)

rimuoviPos :: (Int -> Bool) -> [a] -> [a]
rimuoviPos cond xs = map fst $ filter (cond . snd) $ zip xs [1..]

rimuoviPosPari :: [a] -> [a]
rimuoviPosPari = rimuoviPos odd

rimuoviPosDispari :: [a] -> [a]
rimuoviPosDispari = rimuoviPos even

sommaPosDispari :: (Num a) => [a] -> a
sommaPosDispari = sum . rimuoviPosPari

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort $ filter (<=x) xs) ++ [x] ++ (quickSort $ filter (>x) xs)

dueMinori :: Ord a => [a] -> (Maybe a, Maybe a)
dueMinori [] = (Nothing, Nothing)
dueMinori (x:[]) = (Just x, Nothing)
dueMinori xs = (Just a, Just b) where (a:b:_) = (quickSort xs)

dispari :: [Integer] -> [Integer]
dispari = filter odd

dueMinoriDispari :: [Integer] -> (Maybe Integer, Maybe Integer)
dueMinoriDispari = dueMinori . dispari

coppieConSommeSuffisse :: Num b => [b] -> [(b, b)]
coppieConSommeSuffisse xs = zip xs (tail $ scanr(+) 0 xs)

coppieConSommePrefisse :: Num b => [b] -> [(b, b)]
coppieConSommePrefisse xs = zip xs (scanl(+) 0 xs)

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
    row_dim xs
      | allSame = (head dims)
      | otherwise = -1
      where
        dims = map length xs
        allSame = all (== (head dims)) dims
    r = length mat
    c = row_dim mat

opElementwise :: (a -> a -> a) -> [a] -> [a] -> [a]
opElementwise op a b = map(\(x,y) -> op x y) $ zip a b

sumVec :: Num a => [a] -> [a] -> [a]
sumVec = opElementwise (+)
minVec :: (Ord a) => [a] -> [a] -> [a]
minVec = opElementwise min
maxVec :: (Ord a) => [a] -> [a] -> [a]
maxVec = opElementwise max
prodVec :: (Num a) => [a] -> [a] -> [a]
prodVec = opElementwise (*)

opColumnsAccumulation :: ([a] -> [a] -> [a]) -> [[a]] -> [a]
opColumnsAccumulation op = foldl1(\acc x -> op acc x)

colsums :: Num a => [[a]] -> [a]
colsums = opColumnsAccumulation sumVec

invertiSegno :: Num a => [a] -> [a]
invertiSegno = map (\x -> -x)

colaltsums :: Num a => [[a]] -> [a]
colaltsums v = sumVec (colsums $ rimuoviPosPari v) (colsums $ map invertiSegno $ rimuoviPosDispari v)

colminmax :: Ord b => [[b]] -> [(b, b)]
colminmax mat = zip (opColumnsAccumulation minVec mat) (opColumnsAccumulation maxVec mat)

prodottoCartesiano :: [a] -> [b] -> [(a, b)]
prodottoCartesiano a b = [(x,y)| x<-a, y<-b]

allZero :: (Eq a, Num a) => (Int -> Int -> Bool) ->  [[a]] -> Bool
allZero pred m = all (==0) . map snd .filter (\((r,c), _) -> pred r c)
  $ zip (prodottoCartesiano [0..((length m) - 1)] [0..((length m) - 1)]) (concat m)

lowertriangular :: (Eq a, Num a) => [[a]] -> Bool
lowertriangular = allZero (>)

uppertriangular :: (Eq a, Num a) => [[a]] -> Bool
uppertriangular m = allZero (<) m

diagonal :: (Eq a, Num a) => [[a]] -> Bool
diagonal m = allZero (/=) m

convergent :: (Ord a, Num a) => [[a]] -> a -> Bool
convergent = convergentAux (0:(cycle [1]))
  where convergentAux :: (Ord a, Num a) => [a] -> [[a]] -> a -> Bool
        convergentAux _ [] _ = True
        convergentAux v (x:xs) r = ((<r) $ abs $ sum $ prodVec x v) && (convergentAux (1:v) xs r)


v = [[1,2,3], [0, 2, 4], [0, 0, 1]]
--[-6,0,-1]
