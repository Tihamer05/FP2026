-- I
--Atlag

atlag :: (Floating a) => [a] -> a
atlag ls = (sum ls) / fromIntegral (length ls)

-- II

-- meghatározza egy lista elemszámát, 2 módszerrel (myLength)

myLength1 :: (Floating a) => [a] -> Int
myLenght1 [] = 0
myLength1 (_:xs) = 1 + myLength1 xs

--myLength2 :: (Floating a) => [a] -> Int   
myLength2 :: Num a => [b] -> a -> a
myLength2 [] res = res
myLength2 (x:xs) res = myLength2 xs (res + 1)

myLenght3 :: (Foldable t, Num b) => t a -> b
myLenght3 ls = foldr(\x -> (+) 1) 0 ls

myLength4 :: (Foldable t, Num b) => t a -> b
myLength4 ls = foldl (\db x -> (+) 1 db) 0 ls

myLenght5 :: Foldable t => t a -> Int
myLenght5 ls = length ls
 
-- összeszorozza a lista elemeit, 2 módszerrel (myProduct)
myProduct1 :: Num a => [a] -> a
myProduct1 [] = 1
myProduct1 (x:xs) = x * myProduct1 xs


myProduct2 :: Num t => [t] -> t -> t
myProduct2 [] res = res
myProduct2 (x:xs) res = myProduct2 xs (res * x)

myProduct3 :: (Foldable t, Num a) => t a -> a
myProduct3 ls = foldr1 (*) ls


-- meghatározza egy lista legkisebb elemét (myMinimum)

myMin :: Ord a1 => [a1] -> a2
myMin [] = error "Ures lista"
myMin (x1:x2:xs)
    | x1 < x2 = myMin (x1 : xs)
    | otherwise = myMin (x2: xs)


myMin2 :: Ord a1 => [a1] -> a2
myMin2 [] = error "Ures lista"
myMin2 (x1:x2:xs) = if x1 < x2 then myMin2 (x1:xs) else myMin2 (x2:xs)

myMin3 :: (Foldable t, Ord a) => t a -> a
myMin3 ls = foldl1 min ls

--meghatározza egy lista legnagyobb elemét (myMaximum)

myMax :: Ord a => [a] -> a
myMax [] = error "Ures lista"
myMax [x] = x
myMax (x1:x2:xs) 
    | x1 > x2 = myMax (x1:xs)
    | otherwise = myMax (x2:xs)

myMax1 :: (Foldable t, Ord a) => t a -> a
myMax1 ls = foldl1 max ls

--meghatározza egy lista n-ik elemét 

myIterator :: [a] -> Int -> a
myIterator ls n = ls !! n

myIterator2 ls n
    | ls == [] = error "ures"
    | n < 0 = error "hiba"
    | length ls <= n = error "tul nagy"
    | otherwise = ls !! n

-- egymásután fűzi a paraméterként megadott két listát
lsFuz :: [a] -> [a] -> [a]
lsFuz ls1 ls2 = ls1 ++ ls2

-- megállapítja egy listáról, hogy az palindrom-e vagy sem
palindrome :: Eq a => [a] -> Bool
palindrome ls = ls == reverse ls

palindrome1 :: Eq a => [a] -> Bool
palindrome1 ls = head ls == last ls && palindrome ((init . tail) ls)

-- meghatározza egy egész szám számjegyeinek listáját
szjLs :: Integral a => a -> [a]
szjLs x
    | x < 0 = szjLs (abs x)
    | x < 10 = [x]
    | otherwise = szjLs (div x 10) ++  [mod x 10]

-- a lista első elemét elköltözteti a lista végére
elsoUtolso :: [a] -> [a]
elsoUtolso ls = tail ls ++ [head ls]

elsoUtolso1 :: [a] -> [a]
elsoUtolso1 (x:xs) = xs ++ [x]

-- meghatározza egy 10-es számrendszerbeli szám p számrendszerbeli alakját
decP x p
    | x < 0 = error "Negativ"
    | x < p = [x]
    | otherwise = decP (div x p) p ++ [mod x p]

-- meghatározza egy p számrendszerben megadott szám számjegyei alapján a megfelelő 10-es számrendszerbeli számot.
pDec :: (Foldable t, Num b) => t b -> b -> b
pDec ls p = foldl (\hatvany x -> x + (p * hatvany)) 0 ls

pDec2 :: Integral a => a -> a -> a
pDec2 x p = 
    let 
        szamjegyek x
            | x < 10 = [x]
            | otherwise = mod x 10 : szamjegyek (div x 10)
        szjIdx = zip (szamjegyek x) [0..]
    in sum [i * (p * hatvany) | (i, hatvany) <- szjIdx]

-- Map hasznalata

listaNMap :: Eq b => [[b]] -> [b]
listaNMap ls = map (\x -> myIterator2 x 0)  ls

ls3 = [[1..10], [5.66]]

-- Polinomok

poli :: Num t => [t] -> t -> t
poli [] x = 1
poli (a : aLs) x = a + x * (poli aLs x)

type Pont = (Double, Double)

lsP :: [Pont]

lsP = [(3.4, 2.4), (8.7, 1.2), (4, 5), (1.2, 23.8)]

p :: Pont
p = (3.4, 1.7)

tavolsag :: Floating a => (a, a) -> (a, a) -> a
tavolsag (x1, y1) (x2, y2) = sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)

minTavolsag :: (Foldable t, Ord a, Floating a) => t (a, a) -> (a, a) -> (a, a)
minTavolsag lsP p = foldl1 aux lsP
  where
    aux p1 p2 = if tavolsag p1 p < tavolsag p2 p then p1 else p2

minTavolsag3 :: (Foldable t, Ord a, Floating a) => t (a, a) -> (a, a) -> (a, a)
minTavolsag3 lsP p = foldl1 (\p1 p2 -> if tavolsag p1 p < tavolsag p2 p then p1 else p2) lsP

minTavolsag2 :: (Ord a, Floating a) => [(a, a)] -> (a, a) -> (a, a)
minTavolsag2 [] _ = error "ures lista"
minTavolsag2 [p1] _ = p1
minTavolsag2 (p1 : p2 : lsP) p
  | tavolsag p1 p < tavolsag p2 p = minTavolsag2 (p1 : lsP) p
  | otherwise = minTavolsag2 (p2 : lsP) p   