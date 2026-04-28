import Data.Char
import Data.List
import Text.Printf
import Data.Ord (comparing)

-- Szamtech 2025

getBiggerGrades ls n = [x | x <- ls, x > n]

getSmallerGrades ls n = [x | x <- ls, x < n]

countEqualGrades ls n = length . filter (== n) $ ls

szamtech1 = do
    let ls = [9.5, 10, 6.5, 4.5, 5.5, 7, 7.25, 7.8, 9, 3.5, 6.5, 9.75, 3.5]
        n = 6.5
        bls = getBiggerGrades ls n 
        sls = getSmallerGrades ls n
        nr = countEqualGrades ls n 
    
    if not (null bls) then do
        putStr "Nagyobbak: "
        mapM_ (\x -> putStr (show x ++ ", ")) bls
        putStrLn (" szamuk: " ++ show (length bls))
    else 
        putStrLn "Nagyobbak: nincsenek"

    if not (null sls) then do
        putStr "Kisebbek: "
        mapM_ (\x -> putStr (show x ++ ", ")) sls
        putStrLn (" szamuk: " ++ show (length sls))
    else 
        putStrLn "Kisebbek: nincsenek"

    if nr /= 0 then do
        putStrLn ("Megegyezik " ++ show nr)
    else
        putStrLn "Nincs megegyezo"

ex_a ls = 
    let minDay = minimum [ drop 3 d | (_, d, _) <- ls]
    in [(a, b,c ) | (a, b, c) <- ls, drop 3 b == minDay] 


ex_b email =
    let user = takeWhile (/= '@') email 
        rest = tail (dropWhile (/= '@') email)
        szerver = takeWhile (/= '.') rest 
        domain = tail( dropWhile (/= '.') rest)
    in user ++ " " ++ szerver ++ " " ++ domain

szamtech2 = do
    let lsE = [("rosalesanthony@gmail.com", "03/31", "213130957725524"),
                ("robin18@example.net", "02/29", "570620146482"),
                ("bsullivan@example.org", "03/27", "4215057708441701869"),
                ("jameshughes@example.org", "09/27", "4782851642138996"),
                ("douglasjordan@yahoo.com", "03/27", "5289954454350249"),
                ("jwells@example.net", "06/31", "342926219737676"),
                ("spotter@gmail.com", "01/27", "4917299108623093")]
    
        ls = ex_a lsE
    mapM_ (\(a, b, c) -> putStrLn (a ++ " " ++ b ++ " " ++ c)) ls
    putStrLn "\n"
    mapM_ (\(e,_,_) -> putStrLn (ex_b  e)) lsE

--Tetel 1

--1
count s = length [c | c <- s, toLower c  `elem` "aeiouáéíóöőúüű"]

fel1 = do
    input <- getLine

    let szavak = words input
    let temp = map count szavak
    let max = maximum temp

    print [s | s <- szavak, count s == max]

--2
fiboVegtelen = fiboSg 0 1 0
    where
        fiboSg a b res = res : fiboSg b res (res + b)

getFibs indexek = [fiboVegtelen !! i | i <- indexek]

fel2 = do
    let ls = [10, 25, 1000, 0, 15, 5000]
    print $ getFibs ls

-- fel3

valogat varosok n = sort [nev | (nev, ertek) <- varosok, ertek > n]

fel3 = do
    let ls = [("sepsiszentgyorgy",54000),("kolozsvár",330000),("marosvasarhely",130000),("temesvar",310000),("arad",160000),("gyergyoszentmiklos",18000),("nagyvarad",196000)]
        varosok = valogat ls 54500

    putStrLn $ intercalate ", " varosok

-- Tetel 2

-- 1
isHamming :: Integer -> Bool
isHamming 0 = False
isHamming 1 = True
isHamming n
    | n `mod` 2 == 0 = isHamming (n `div` 2)
    | n `mod` 3 == 0 = isHamming (n `div` 3)
    | n `mod` 5 == 0 = isHamming (n `div` 5)
    | otherwise      = False

haming = do
    l1 <- readLn :: IO Integer
    l2 <- readLn :: IO Integer

    let result = [x | x <- [l1..l2], isHamming x]
    mapM_ print result

-- Tetel 3

ketoldaliSzomszedok :: Ord p => p -> [p] -> [p]
ketoldaliSzomszedok s lsS = aux (sort lsS)
    where 
        aux (x : y : z : ve)
            | x == s = [y]
            | y == s = [x, z]
            | z == s = [y]
            | otherwise = aux ve

ketoldaliSzomszedok2 :: Eq a => a -> [a] -> [a]
ketoldaliSzomszedok2 s lsS
    | index == 0 = [lsS !! (index + 1)]
    | index == length lsS - 1 = [lsS !! (index - 1)]
    | otherwise = [lsS !! (index -1), lsS !! (index + 1)]
    where
        index = aux (zip [0 ..] lsS)
        aux ((idx, nev): ve)
            | nev == s = idx 
            | otherwise = aux ve

mainSzomszedok :: IO()
mainSzomszedok = do
    let s = "Feri"
        lsS = ["Mari", "Zsuzsa", "Szidi", "Lori", "Kata", "Feri", "Teri", "Dani", "Zsolti"]
        szomszedok = ketoldaliSzomszedok2 s lsS 
        sz1 = head szomszedok
        sz2 = last szomszedok
    if sz1 == sz2
        then putStrLn (s ++ "1 szomszedja van, " ++ sz1)
        else putStrLn (s ++ " bal oldal: " ++ sz1 ++ ", jobb oldal: " ++ sz2)

-- 2
nullabanVegzodok ls = [x | x <- ls, x `mod` 10 == 0]

nullaMain :: IO()
nullaMain = do
    let ls = [10, 21, 31, 2130, 12]
        temp = nullabanVegzodok ls
    
    mapM_ print temp

--3
maxErtekLista ls = let maxErtek = maximum [d | (_,d,_) <- ls]
                    in (maxErtek, [a | (a, b, _) <- ls, b == maxErtek])

main3 = do
    let ls = [("iphoneS1",20,2500), ("huaweiS1",30,1700), ("huaweiS2",25,3100),("samsungA1",30,2000), ("nokia",10,1900), ("iphoneS2",10,2200), ("samsungA2",15,1650), ("iphone3",30,1800)]
        temp = maxErtekLista ls

    print (fst temp)
    putStrLn $ intercalate ", " (snd temp)

-- Tetel 4
primeFactors :: Integer -> [Integer]
primeFactors n = factors n 2
  where
    factors n d
      | n < 2          = []
      | n `mod` d == 0 = d : factors (n `div` d) d
      | d * d > n      = [n]
      | otherwise      = factors n (d + 1)

-- 2. Fő logika
feladat = do
    putStr "n = "
    line <- getLine
    let n = read line :: Integer
    
    if n < 2 
        then putStrLn "Adj meg 2-nél nagyobb számot!"
        else do
            let factors = primeFactors n
            let csoportok = group factors
            
            let legjobbCsoport = maximumBy (comparing length) csoportok
            
            let prim = head legjobbCsoport
            let kitevo = length legjobbCsoport
            
            putStrLn $ "Az eredmény: " ++ show prim ++ ", mert ez a(z) " ++ 
                       show kitevo ++ ". hatványon szerepel."


-- Tetel 5
nincsNull n 
    | n < 10 && n == 0 = False  
    | n < 10 && n /= 0 = True
    | mod n 10 == 0 = False
    | otherwise = nincsNull (div n 10)

fel2Main :: IO()
fel2Main = do
    let ls = [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
        ls1 = filter nincsNull ls
        ls2 = filter (\n -> '0' `notElem` show n) ls
        ls3 = filter nincsNull ls
    if null ls3
        then putStrLn "Nincs ilyen!"
        else do
            putStrLn "Talalnak: "
            mapM_ (\n -> putStr (show n ++ " ")) ls1

--Tetel 5
stringIsmetles = do
    let n = 7
    let szavak = ["SZIA", "A", "B"]
    let eredmenyLista = take n (cycle szavak)

    mapM_  putStr (take n (cycle szavak))

-- Fibo 
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

kiirFib :: Int -> IO ()
kiirFib n = do
    let fibErtek = fibs !! n
    printf "      %d: %d\n" n fibErtek

fibFeladat :: IO ()
fibFeladat = do
    let bemenet = [10, 25, 1000, 0, 15, 5000]
    mapM_ kiirFib bemenet

main = fibFeladat