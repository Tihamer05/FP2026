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