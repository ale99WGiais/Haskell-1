module Esercizi where

  --1.1
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

--1.2
combinazioni :: Integer -> Integer -> Integer
combinazioni n k = div (factorial n)  ((factorial k) * (factorial (n-k)))

--1.3
combinazioniElementi :: (Eq a) => [a] -> [[a]]
combinazioniElementi [] = [[]]
combinazioniElementi xs = foldr (\x acc -> x ++ acc) [] (map (\x -> (map (\y -> x:y) (combinazioniElementi [t | t <- xs, t /= x]))) xs)

rimuoviPos :: (Int -> Bool) -> [a] -> [a]
rimuoviPos cond xs = map fst $ filter (cond . snd) $ zip xs [1..]

rimuoviPosPari :: [a] -> [a]
rimuoviPosPari = rimuoviPos odd

rimuoviPosDispari :: [a] -> [a]
rimuoviPosDispari = rimuoviPos even

--2.2
sommaPosDispari :: (Num a) => [a] -> a
sommaPosDispari = sum . rimuoviPosPari

--2.3
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort $ filter (<=x) xs) ++ [x] ++ (quickSort $ filter (>x) xs)

dueMinori :: Ord a => [a] -> (Maybe a, Maybe a)
dueMinori [] = (Nothing, Nothing)
dueMinori (x:[]) = (Just x, Nothing)
dueMinori xs = (Just a, Just b) where (a:b:_) = (quickSort xs)

dispari :: [Integer] -> [Integer]
dispari = filter odd

--2.4
dueMinoriDispari :: [Integer] -> (Maybe Integer, Maybe Integer)
dueMinoriDispari = dueMinori . dispari

--2.5
coppieConSommeSuffisse :: Num b => [b] -> [(b, b)]
coppieConSommeSuffisse xs = zip xs (tail $ scanr(+) 0 xs)

--2.6
coppieConSommePrefisse :: Num b => [b] -> [(b, b)]
coppieConSommePrefisse xs = zip xs (scanl(+) 0 xs)

--2.7
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

--3.1
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

--3.2
colsums :: Num a => [[a]] -> [a]
colsums = opColumnsAccumulation sumVec

invertiSegno :: Num a => [a] -> [a]
invertiSegno = map (\x -> -x)

--3.3
colaltsums :: Num a => [[a]] -> [a]
colaltsums v = sumVec (colsums $ rimuoviPosPari v) (colsums $ map invertiSegno $ rimuoviPosDispari v)

--3.4
colminmax :: Ord b => [[b]] -> [(b, b)]
colminmax mat = zip (opColumnsAccumulation minVec mat) (opColumnsAccumulation maxVec mat)

prodottoCartesiano :: [a] -> [b] -> [(a, b)]
prodottoCartesiano a b = [(x,y)| x<-a, y<-b]

--lista con elementi di m la cui posizione rispetta pred
filterMatrix pred m = map snd . filter (\((r,c), _) -> pred r c)
                        $ zip (prodottoCartesiano [0..((length m) - 1)] [0..((length m) - 1)]) (concat m)

allZero pred m = all (==0) $ filterMatrix pred m

--3.5
lowertriangular :: (Eq a, Num a) => [[a]] -> Bool
lowertriangular = allZero (>)

--3.6
uppertriangular :: (Eq a, Num a) => [[a]] -> Bool
uppertriangular m = allZero (<) m

--3.7
diagonal :: (Eq a, Num a) => [[a]] -> Bool
diagonal m = allZero (/=) m

--3.8
convergent :: (Ord a, Num a) => [[a]] -> a -> Bool
convergent = convergentAux (0:(cycle [1]))
  where convergentAux :: (Ord a, Num a) => [a] -> [[a]] -> a -> Bool
        convergentAux _ [] _ = True
        convergentAux v (x:xs) r = ((<r) $ abs $ sum $ prodVec x v) && (convergentAux (1:v) xs r)

--3.9
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose (x:[]) = [[t] | t <- x]
transpose (x:xs) = map (\(l, e) -> e:l) $ zip (transpose xs) x

--3.10
isSymmetric :: Eq a => [[a]] -> Bool
isSymmetric m = m == (transpose m)

toMatrix :: Int -> [a] -> [[a]]
toMatrix _ [] = []
toMatrix ncols m = (take ncols m):(toMatrix ncols (drop ncols m))

--3.11
matProduct :: Num a => [[a]] -> [[a]] -> [[a]]
matProduct a b = toMatrix (length $ head b) $ map (\(a, b) -> sum $ prodVec a b) $ prodottoCartesiano a (transpose b)

data BST a = Void | Node {
  val :: a,
  left, right :: BST a
} deriving (Eq, Ord, Read, Show)

--4.1
sommaAlbero :: Num p => BST p -> p
sommaAlbero Void = 0
sommaAlbero n = val n + (sommaAlbero $ left n) + (sommaAlbero $ right n)

--4.2
sommaDispariAlbero Void = 0
sommaDispariAlbero n = (if odd v then v else 0) + (sommaDispariAlbero $ left n) + (sommaDispariAlbero $ right n)
  where v = val n

--4.3
samesums :: (Eq a, Num a) => [BST a] -> Bool
samesums v = allSame $ map sommaAlbero $ v

--4.4
bstelem :: (Ord a, Eq a) => a -> BST a -> Bool
_ `bstelem` Void = False
x `bstelem` n
  | x == cur = True
  | x < cur = x `bstelem` (left n)
  | otherwise = x `bstelem` (right n)
  where cur = val n

--4.5
bstinsert :: (Ord a, Eq a) => a -> BST a -> BST a
bstinsert x Void = Node {val=x, left=Void, right=Void}
bstinsert x node
  | x < cur = Node{ val=(val node), left=(bstinsert x (left node)), right= (right node)}
  | otherwise = Node{ val=(val node), left=(left node), right= (bstinsert x (right node))}
  where cur = val node

--4.6
bstToVec :: BST a -> [a]
bstToVec Void = []
bstToVec node = (bstToVec $ left node) ++ [val node] ++ (bstToVec $ right node)

vecToBst :: Ord a => [a] -> BST a
vecToBst = foldl(\acc x -> bstinsert x acc) Void

--4.7
treeord :: Ord a => [a] -> [a]
treeord = bstToVec . vecToBst

--4.8
filtertree :: (a -> Bool) -> BST a -> [a]
filtertree p = (filter p) . bstToVec

nodeh :: BST (a, Integer) -> Integer
nodeh Void = 0
nodeh x = snd $ val x

--4.9
annotate :: (Ord a) => BST a -> BST (a, Integer)
annotate Void = Void
annotate node = Node { val = (val node, h+1), left=l, right=r}
  where
    l = annotate $ left node
    r = annotate $ right node
    h = max (nodeh l) (nodeh r)

--4.10
almostBalanced :: Ord a => BST a -> Bool
almostBalanced x = aux $ annotate x
  where
    aux Void = True
    aux x = ok && (aux $ left x) && (aux $ right x)
      where
        lh = nodeh $ left x
        rh = nodeh $ right x
        ok = abs (lh-rh) < 2

data WBST a = WVoid | WNode a Int ( WBST a) ( WBST a) deriving (Read, Show)

--4.11
wbstinsert x WVoid = WNode x 1 WVoid WVoid
wbstinsert x (WNode val h left right) = WNode val h l r
  where
    l = if x < val then wbstinsert x left else left
    r = if x >= val then wbstinsert x right else right
    height WVoid = 0
    height (WNode _ h _ _) = h
    h = (max (height l) (height r)) + 1

a = foldl(\acc x -> bstinsert x acc) Void [2,3,0,8,6]
b = foldl(\acc x -> wbstinsert x acc) WVoid [2,3,0,5,4]

--4.12
diff2next tree = fst $ aux tree diffs
  where
    visitOrd = bstToVec tree
    diffs = (map (\(a,b) -> Just (b-a)) $ zip visitOrd (tail visitOrd)) ++ [Nothing]
    aux Void diffs = (Void, diffs)
    aux node diffs = (newNode, newDiffs)
      where
        (newNodeL, newDiffsL) = aux (left node) diffs
        newNode = Node {val = (val node, head newDiffsL), left = newNodeL, right = newNodeR}
        (newNodeR, newDiffs) = aux (right node) (tail newDiffsL)

--4.13
bstlevels :: Ord a => BST a -> [a]
bstlevels tree = map snd $ treeord $ map (\(a,b) -> (-b,a)) $ bstToVec $ annotate tree

fold :: (Ord a) => (a -> b -> b -> b) -> b -> BST a -> b
fold _ z Void = z
fold f z ( Node x l r) = f x ( fold f z l) ( fold f z r)

--4.14
treeheight' tree = fold (\x l r -> 1 + (max l r)) 0 tree

--4.15
annotate' tree = fold (\x l r -> Node {val=(x, 1+(max (nodeh l) (nodeh r))), left=l, right=r}) Void $ tree

--4.16
almostBalanced' :: Ord a => BST a -> Bool
almostBalanced' Void = True
almostBalanced' tree = fst $ val $  fold aux Void $ tree
  where
    aux x l r = Node {val=(ok , 1+(max (nodeh l) (nodeh r))), left=l, right=r}
      where
        lh = nodeh l
        rh = nodeh r
        ok = (abs (lh-rh) < 2) && (fst $ val l) && (fst $ val r)

diameter :: Ord a => BST a -> Integer
diameter tree = fst $ fold (\x l r -> (maxh x l r, h x l r)) (0, -1) tree
  where
    maxh x l r = maximum [fst l, fst r, 2 + (snd l) + (snd r)]
    h x l r = 1 + (max (snd l) (snd r))

--4.17
maxDiameter :: Ord a => [BST a] -> Integer
maxDiameter = maximum . map diameter

--4.18
isBst :: Ord a => BST a -> Bool
isBst tree = ok' $ fold (\x l r -> ((isok x l r, True), (newmin x l r, newmax x l r))) ((True, False), (val tree, val tree)) tree
  where
    min' = fst . snd
    max' = snd . snd
    ok' = fst . fst
    present' = snd . fst
    isok x l r
      | (not (ok' l)) || (not (ok' r)) = False
      | (present' l) && (x < max' l) = False
      | (present' r) && (x > min' r) = False
      | otherwise = True
    newmin x l r = if present' l then min' l else x
    newmax x l r = if present' r then max' r else x

data ABST a = AVoid | ANode Bal a ( ABST a) ( ABST a) deriving (Eq , Ord , Read , Show )
data Bal = Left | Bal | Right deriving (Eq , Ord , Read , Show )


-- todo alberi ricerca 19, 20

--4.21
bst2List' :: Ord a => BST a -> [a]
bst2List' tree = fold (\x l r -> l ++ [x] ++ r) [] tree

--filtertree' :: (a -> Bool) -> BST a -> [a]
--filtertree' p = fold (\x l r -> l ++ (if p x then [x] else []) ++ r) []



-- Alberi generici

data (Eq a, Show a) => Tree a = Void | Node a [Tree a]
  deriving (Eq, Show)

treefold :: (Eq a, Show a) => (a -> [b] -> b) -> b -> Tree a -> b
treefold =

--a = [[1,2,3], [1, 1, 1]]
--b = [[5,1], [4, 1], [1, 2]]

--v = [[1,2], [0, 2], [0, 0]]
--[-6,0,-1]
