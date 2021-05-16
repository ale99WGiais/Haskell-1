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

allSame :: (Eq a) => [a] -> Bool
allSame v = all (== (head v)) v

matrix_dim :: [[a]] -> (Int, Int)
matrix_dim mat
  | c == -1 = (-1, -1)
  | otherwise = (r, c)
  where
    row_dim xs = if allSame dims then (head dims) else -1
      where
        dims = map length xs
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

--lista con elementi di m la cui posizione rispetta pred
filterMatrix pred m = map snd . filter (\((r,c), _) -> pred r c)
                        $ zip (prodottoCartesiano [0..((length m) - 1)] [0..((length m) - 1)]) (concat m)

allZero pred m = all (==0) $ filterMatrix pred m

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

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose (x:[]) = [[t] | t <- x]
transpose (x:xs) = map (\(l, e) -> e:l) $ zip (transpose xs) x

isSymmetric :: Eq a => [[a]] -> Bool
isSymmetric m = m == (transpose m)

toMatrix :: Int -> [a] -> [[a]]
toMatrix _ [] = []
toMatrix ncols m = (take ncols m):(toMatrix ncols (drop ncols m))

matProduct :: Num a => [[a]] -> [[a]] -> [[a]]
matProduct a b = toMatrix (length $ head b) $ map (\(a, b) -> sum $ prodVec a b) $ prodottoCartesiano a (transpose b)

data BST a = Void | Node {
  val :: a,
  left, right :: BST a
} deriving (Eq, Ord, Read, Show)

sommaAlbero :: Num p => BST p -> p
sommaAlbero Void = 0
sommaAlbero n = val n + (sommaAlbero $ left n) + (sommaAlbero $ right n)

a = Node {val=1, left=Node {val=0, right=Void, left=Void}, right=Node {val=2, left=Void, right=Void}}

sommaDispariAlbero Void = 0
sommaDispariAlbero n = (if odd v then v else 0) + (sommaDispariAlbero $ left n) + (sommaDispariAlbero $ right n)
  where v = val n

samesums :: (Eq a, Num a) => [BST a] -> Bool
samesums v = allSame $ map sommaAlbero $ v

bstelem :: (Ord a, Eq a) => a -> BST a -> Bool
_ `bstelem` Void = False
x `bstelem` n
  | x == cur = True
  | x < cur = x `bstelem` (left n)
  | otherwise = x `bstelem` (right n)
  where cur = val n

bstinsert :: (Ord a, Eq a) => a -> BST a -> BST a
bstinsert x Void = Node {val=x, left=Void, right=Void}
bstinsert x node
  | x < cur = Node{ val=(val node), left=(bstinsert x (left node)), right= (right node)}
  | otherwise = Node{ val=(val node), left=(left node), right= (bstinsert x (right node))}
  where cur = val node

bstToVec :: BST a -> [a]
bstToVec Void = []
bstToVec node = (bstToVec $ left node) ++ [val node] ++ (bstToVec $ right node)

vecToBst :: Ord a => [a] -> BST a
vecToBst [] = Void
vecToBst (x:xs) = bstinsert x (vecToBst xs)

treeord :: Ord a => [a] -> [a]
treeord = bstToVec . vecToBst

--a = [[1,2,3], [1, 1, 1]]
--b = [[5,1], [4, 1], [1, 2]]

--v = [[1,2], [0, 2], [0, 0]]
--[-6,0,-1]
