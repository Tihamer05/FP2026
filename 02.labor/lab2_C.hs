-- egy szám számjegyeinek szorzatát (2 módszerrel)
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

szorzatSzamjegy :: Integral a => a -> a
szorzatSzamjegy nr
    | nr < 0 = szorzatSzamjegy (-nr)
    | nr < 10   = nr
    | otherwise = (nr `mod` 10) * szorzatSzamjegy (nr `div` 10)

szorzatSzamjegy_ :: Integral a => a -> a
szorzatSzamjegy_ nr 
    | nr == 0 = 0
    | otherwise = go (abs nr) 1
    where 
        go 0 acc = acc
        go x acc = go (x `div` 10 ) (acc * (x `mod` 10))

-- egy szám számjegyeinek összegét
osszSzamjegy :: Int -> Int
osszSzamjegy nr
    | nr < 0 = osszSzamjegy (-nr)
    | nr < 10 = nr
    | otherwise = mod nr 10 + osszSzamjegy (div nr 10)

osszSzamjegy_ :: Int -> Int
osszSzamjegy_ nr
    | nr == 0 = 0
    | otherwise = go (abs nr) 0
    where
        go 0 acc = acc
        go x acc = go (div x 10) (acc + mod x 10)

-- egy szám számjegyeinek számát (2 módszerrel)

szamjegyekSzama :: Int -> Int
szamjegyekSzama nr
    | nr < 10 = 1
    | otherwise = 1 + szamjegyekSzama (div nr 10)

szamjegyekSzama_ :: Int -> Int
szamjegyekSzama_ 0 = 1
szamjegyekSzama_ nr = go (abs nr) 0
    where 
        go 0 acc = acc
        go x acc = go (div x 10) (acc + 1)

-- egy szám azon számjegyeinek összegét, mely paraméterként van megadva

fugv4 :: Int -> Int -> Int
fugv4 nr temp
    | nr == 0 = 0
    | nr == temp = nr
    | mod nr 10 == temp = temp + fugv4 (div nr 10) temp
    | otherwise = fugv4 (div nr 10) temp

-- egy szám páros számjegyeinek számát
parosSzamjegy :: Int -> Int
parosSzamjegy nr
    | nr == 0 = 0
    | mod nr 2 == 0 = 1 + parosSzamjegy (div nr 10)
    | otherwise = parosSzamjegy (div nr 10)

-- egy szám legnagyobb számjegyét
legnagyobbSzamjegy :: Int -> Int
legnagyobbSzamjegy nr 
    | nr < 10 = nr
    | otherwise = max (mod nr 10) (legnagyobbSzamjegy (div nr 10))

-- szamrendszerek
szamrendszer :: Int -> Int -> Int -> Int
szamrendszer nr b n
    | nr == 0 && n == 0 = 1
    | nr == 0 = 0
    | otherwise = (if mod nr b == n then 1 else 0) + szamrendszer (div nr 10) b n

-- n-edik Fibonacci szam
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib_ :: Int -> Int
fib_ n = go n 0 1
    where 
        go 0 a b = a
        go n a b = go (n-1) b (a+b) 

main :: IO ()
main = do
    --putStrLn "Szamjegyek szorzata:"
    --print (szorzatSzamjegy_ 1223)

    -- putStrLn "Szamjegyek osszege:"
    -- print (osszSzamjegy_ 1142) 

    -- putStrLn "Szamjegyek szama:"
    -- print(szamjegyekSzama_ 12372349131)

    print(fugv4 577712 7)


