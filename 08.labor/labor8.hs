import Data.List

--I

data Fesztivalok = Fesztivalok {
    fEgyuttes :: String,
    fFesztival :: String,
    fAr :: Int,
    fKod :: Int
} deriving(Show)

fesztivalok :: [Fesztivalok]
fesztivalok =
  [ Fesztivalok "Coldplay" "Glastonbury" 5000 101,
    Fesztivalok "Radiohead" "Coachella" 4500 102,
    Fesztivalok "Arctic Monkeys" "Glastonbury" 6000 103,
    Fesztivalok "Foo Fighters" "Lollapalooza" 4000 104,
    Fesztivalok "The Killers" "Coachella" 5500 105,
    Fesztivalok "Muse" "Lollapalooza" 3000 106,
    Fesztivalok "Imagine Dragons" "Glastonbury" 7000 107,
    Fesztivalok "Tame Impala" "Coachella" 3500 108,
    Fesztivalok "Red Hot Chili Peppers" "Lollapalooza" 8000 109,
    Fesztivalok "The Strokes" "Glastonbury" 2500 110
  ]


keresEgyuttesek :: [Fesztivalok] -> String -> [String]
keresEgyuttesek ls fesztival = map fEgyuttes (filter (\f -> fFesztival f == fesztival) ls)

keresEgyuttesek1 :: [Fesztivalok] -> String -> [String]
keresEgyuttesek1 ls fesztival = [fEgyuttes f| f <- ls, fFesztival f == fesztival]

olcsobbJegyek :: [Fesztivalok] -> Int -> [String]
olcsobbJegyek ls ar = [fEgyuttes f| f <-ls, fAr f < ar]

olcsobbJegyekNr :: [Fesztivalok] -> Int -> Int
olcsobbJegyekNr ls ar = length (olcsobbJegyek ls ar)

sortByNev :: [Fesztivalok] -> [Fesztivalok]
sortByNev ls = sortOn (fEgyuttes) ls

sortByAr :: [Fesztivalok] -> [Fesztivalok]
sortByAr = sortOn fAr

-- rendezzük a lista tartalmát összefésülő rendezéssel a kod értékek alapján

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | fKod x <= fKod y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys  

mergeSort [] = []
mergeSort [xs] = [xs]
mergeSort ls = merge (mergeSort bal) (mergeSort jobb)
    where
        (bal, jobb) = splitAt (length ls `div` 2) ls

rendezettFesztivalok :: [Fesztivalok] -> [Fesztivalok]
rendezettFesztivalok ls = mergeSort ls

avgArak ls fesztival = 
    if null arak then 0
    else fromIntegral(sum arak) / fromIntegral(length arak)
    where
        arak = [fAr f | f <- ls, fFesztival f == fesztival]

data Varos = Varos {
    vNev :: String,
    vNepszam :: Int,
    vTerMeret :: Int
} deriving(Show)

splitOnComma s = case dropWhile (==',') s of
    "" -> []
    s' -> w : splitOnComma s''
        where (w, s'') = break (== ',') s'

parseVaros :: [Char] -> Varos
parseVaros sor = 
    let reszek = splitOnComma sor
    in Varos {
        vNev = reszek !! 0,
        vNepszam = read (reszek !! 1),
        vTerMeret = read (reszek !! 2)
    }
nepessegBetween ls a b = length [vNev v | v <- ls, vNepszam v > a, vNepszam v < b]
main1 = do
    tartalom <- readFile "varosok.txt"

    let sorok = lines tartalom
    let varosLista = map parseVaros sorok

    print(varosLista)
