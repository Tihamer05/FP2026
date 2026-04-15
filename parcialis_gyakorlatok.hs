import Data.List
import Data.Char

--1

valogat :: [(String, Int)] -> Int -> [String]
valogat varosok n = sort [nev | (nev, ertek) <- varosok, ertek > n]

fel1 :: IO ()
fel1 = do
    let varosok = [ ("sepsiszentgyorgy", 54000), ("kolozsvár", 330000)
                  , ("marosvasarhely", 130000), ("temesvar", 310000)
                  , ("arad", 160000), ("gyergyoszentmiklos", 18000)
                  , ("nagyvarad", 196000)
                  ]
    
    let n = 150000
    let varosNevek = valogat varosok n

    mapM_ (\v -> putStrLn ("- " ++ v)) varosNevek
    
    putStrLn $ intercalate ", " varosNevek
    
    putStrLn $ unwords varosNevek

--2

nincsNull n 
    | n < 10 && n == 0 = False  
    | n < 10 && n /= 0 = True
    | mod n 10 == 0 = False
    | otherwise = nincsNull (div n 10)

fel2 :: IO()
fel2 = do
    let ls = [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
        ls1 = filter nincsNull ls
        ls2 = filter (\n -> '0' `notElem` show n) ls
        ls3 = filter nincsNull ls
    if null ls3
        then putStrLn "Nincs ilyen!"
        else do
            putStrLn "Talalnak: "
            mapM_ (\n -> putStr (show n ++ " ")) ls1

--3
nincsSzam :: Foldable t => t Char -> Bool
nincsSzam n = not (any isDigit n)


nincsSzam2 :: Foldable t => t Char -> Bool
nincsSzam2 n = all isAlpha n

fel3 = do
    let ls = ["2023tuple", "function", "float", "higher-order", "variable10", "may13be", "0recursion", "monad", "class"]
        ls1 = filter nincsSzam ls 
        ls2 = sort $ filter (\szo -> not (any isDigit szo)) ls
    if null ls1
        then putStrLn "Nincs ilyen"
    else do
        putStrLn "Nem tartalmaznak szamokat: "
        mapM_ putStrLn (sort ls1)

-- 4
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

fel4 :: IO()
fel4 = do
    let s = "Feri"
        lsS = ["Mari", "Zsuzsa", "Szidi", "Lori", "Kata", "Feri", "Teri", "Dani", "Zsolti"]
        szomszedok = ketoldaliSzomszedok2 s lsS 
        sz1 = head szomszedok
        sz2 = last szomszedok
    if sz1 == sz2
        then putStrLn (s ++ "1 szomszedja van, " ++ sz1)
        else putStrLn (s ++ " bal oldal: " ++ sz1 ++ ", jobb oldal: " ++ sz2)

--5

sndThrd (_, x, _) = x

telefonok ls = sort [nev | (nev, eladEr, ar) <- ls, eladEr == maxEladEr ]
    where maxEladEr = maximum (map sndThrd ls)

fel5 :: IO ()
fel5 = do
    let ls = [("iphoneS1", 20, 2500), ("huaweiS1", 30, 1700), ("huaweiS2", 25,3100), ("samsungA1", 30, 2000), ("nokia", 10, 1900), ("iphoneS2", 10, 2200),("samsungA2", 15, 1650), ("iphone3", 30, 1800)]
        maxElement = maximum $ map (\(_,x,_) -> x) ls
        ls1 = telefonok ls
    mapM_ putStrLn ls1


-- 6

-- fel6 = do
--     let ls = [1, 1, 2, 3, 4, 2, 6, 2, 4, 4, 2, 6, 7, 6, 6, 2]
--     megoldas = map(\bLs -> (head bLs, length bLs)) $ filter (\bLs -> odd (length bLs)) $ group $ sort ls