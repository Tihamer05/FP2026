import Data.Char

-- két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát

osszeg :: Num a => a -> a -> a
osszeg a b = a + b

kulonbseg :: Num a => a -> a -> a
kulonbseg a b = a -b

szorzat :: Num a => a -> a -> a
szorzat a b = a * b

szorzat2 a b = (*) a b

hanyados :: Fractional a => a -> a -> a
hanyados a b = a / b

osztasiMaradek ::  Integral a => a -> a -> a
osztasiMaradek a b = mod a b

-- egy első fokú egyenlet gyökét
elsoFokuFuggveny :: Fractional a => a -> a -> a
elsoFokuFuggveny a b = (-b) / a

-- egy szám abszolút értékét
abszolut :: (Num a, Ord a) => a -> a
abszolut a = if a < 0 then -a else a

abszolut2 :: (Num a, Ord a) => a -> a
abszolut2 a
    | a < 0 = -a
    | otherwise = a

-- egy szám előjelét
elojel :: (Ord a, Num a) => a -> String
elojel a = if a < 0 then "Negativ" else if a > 0 then "Pozitiv" else "Nulla"

-- két argumentuma közül a maximumot
max1 :: (Ord a, Num a) => a -> a -> a
max1 a b = if a > b then a else b

-- két argumentuma közül a minimumot
min1 :: (Ord a, Num a) => a -> a -> a
min1 a b  = if a < b then a else b

-- egy másodfokú egyenlet gyökeit
masodFokuFuggveny :: (Ord a, Floating a) => a -> a -> a -> (a, a)
masodFokuFuggveny a b c
    | a == 0 = error "Nem masodfoku egyenlet"
    | a + b + c == 0 = (1, c / a)
    | a - b + c == 0 = (-1, -c / a)
    | delta < 0 = error "komplex szamok"
    | otherwise = (gy1, gy2)
    where
        delta = b^2 - 4*a*c
        gy1 = (-b + sqrt delta) / (2*a)
        gy2 = (-b - sqrt delta) / (2*a)

-- hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől

elempar :: (Eq a) => (a, a) -> (a, a) -> Bool
elempar ep1 ep2 = if (a == c && b == d) || (a == d && c == b) then True else False
    where
        (a, b) = ep1
        (c, d) = ep2

-- az n szám faktoriálisát (3 módszer)
fakt1 :: Int -> Int
fakt1 0 = 1
fakt1 n = n * fakt1(n - 1)

fakt2 :: Int -> Int
fakt2 n
    | n < 0 = -1
    | n == 0 = 1
    | otherwise = n * fakt2 (n - 1)

fakt3 :: Int -> Int -> Int
fakt3 res n
    | n < 0 = error "Negativ szam"
    | n == 0 = res
    | otherwise =  fakt3 (res * n) (n - 1)

-- az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer)

hatvany1 :: (Floating a) => a -> a-> a
hatvany1 x n = x ** n 

hatvany2 :: (Num a, Integral b) => a -> b -> a
hatvany2 x n
    | n == 0 = 1
    | otherwise = x * hatvany2 x (n-1)

hatvany3 :: (Num a, Integral b) => a -> b -> a
hatvany3 _ 0 = 1
hatvany3 x n
    | n `mod` 2 == 0 = fel * fel
    | otherwise = fel * fel * x
    where
        fel = hatvany3 x (n `div` 2)

-- az első n természetes szám negyzetgyökét

negyzetgyok :: (Floating a) => Int -> [a]
negyzetgyok n = [sqrt (fromIntegral x) | x <- [0..(n-1)]]

-- az első n négyzetszámot

negyzetszam :: (Integral a, Enum a) => a -> [a]
negyzetszam n = [x ^ 2 | x <- [1..(n-1)]]

-- az első n természetes szám köbét

kobszam :: (Integral a) => a -> [a]
kobszam n = [x ^ 3 | x <- [1..(n-1)]]

-- az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok

nemnegyzetszam :: (Integral a, Enum a) => a -> [a]
nemnegyzetszam n = [x | x <- [1..(n-1)], not (x `elem` [y^2 | y <- [1..(n-1)]])]

-- x hatványait adott n-ig

szam_hatvany :: (Num a, Enum a) => a -> a -> [a]
szam_hatvany x n = [x * k | k <- [1..n]]

-- egy szám páros osztóinak listáját

paros_osztok :: Integral a => a -> [a]
paros_osztok x = [nr | nr <- [1..x], x `mod` nr == 0, nr `mod` 2 == 0]

-- prim szam ellenorzes
prim :: Integral a => a -> Bool
prim n
    | n < 2 = False
    | otherwise = null [d | d <- [2..(n-1)], n `mod` d == 0]

-- n-ig a prímszámok listáját

prim_szamok :: Int -> [Int]
prim_szamok n = take n [x | x <- [2..], prim x]

-- n-ig az összetett számok listáját

osszetett_szamok :: Int -> [Int]
osszetett_szamok n = take n [x | x <- [0..], prim x == False]

-- n-ig a páratlan összetett számok listáját

paratlan_osszetett :: Int -> [Int]
paratlan_osszetett n = take n [ x | x <- [0..], prim x == False, x `mod` 2 == 1]

-- az n-nél kisebb Pitágorászi számhármasokat

pitagoraszi_szamparok :: Int -> [(Int,Int,Int)]
pitagoraszi_szamparok n = [(a, b, c) | a <- [1..n], b <- [a..n], c <- [b..n], a^2 + b^2 == c^2]

-- Szam es betuparosok

lista = [(chr(i + 97), i) | i <- [0..25]]
lista_ = zip ['a'..'z'] [1..26]

-- Szamparok lista

lista_nr :: (Num b, Enum b) => b -> [(b, b)]
lista_nr n = zip [0..n] [n,(n-1)..0]