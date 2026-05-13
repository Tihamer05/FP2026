import System.IO
import Data.List
import Numeric
import Data.Char
import Control.Monad (forM)
import System.Directory (getFileSize)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC

--az n-nél kisebb négyzetszámokat kiírja egy szövegállományba

negyzetSzamok n = takeWhile (<n) [x ^ 2 | x <- [1..]]

kiirTxt :: IO()
kiirTxt = do
    putStrLn "Add meg n erteket: "
    n <- readLn :: IO Int
    outf <- openFile "ki1.txt" WriteMode
    let ls = negyzetSzamok n 
    hPutStrLn outf (show ls)
    hClose outf
    putStrLn "A kiiratas sikerult"


-- az n-nél kisebb számok négyzetgyökét kiírja egy szövegállományba.
gyokokGeneralas nr xn
    | xnext == xn = xn
    | otherwise = gyokokGeneralas nr xnext
    where 
        xnext = (xn + nr / xn)  / 2

mySqrt nr = gyokokGeneralas nr 1.0

gyokLista n = [mySqrt (fromIntegral x) | x <- [1..n-1]]

kiirGyokok = do
    putStrLn "Add meg n erteket: "
    n <- readLn :: IO Int

    let result = gyokLista n
    --print $ unwords (map show result)
    writeFile "ki2.txt" (unlines $ map show result)

kobGyokokGeneralasa nr xn
    | xnext == xn = xn
    | otherwise = kobGyokokGeneralasa nr xnext
    where xnext = (2 * xn + nr / (xn * xn)) / 3

myKobGyok nr = kobGyokokGeneralasa nr 1.0

kobGyokokLista n = [myKobGyok (fromIntegral x) | x <- [1..n-1]]

kiirKobGyokok = do
    putStrLn "Add meg n erteket: "
    n <- readLn :: IO Int
    let result = kobGyokokLista n 
    writeFile "ki3.txt" (unlines (map show result))

-- II.

toBase nr n = showIntAtBase n intToDigit nr ""

primeFactors :: Int -> [Int]
primeFactors n = factors n 2
  where
    factors 1 _ = []
    factors m k
        | k * k > m = [m]           
        | m `mod` k == 0 = k : factors (m `div` k) k
        | otherwise = factors m (k + 1)

main2 = do
    content <- readFile "input.txt"
    let ls = map read (words content) :: [Int]

    let sorted = sort ls
    writeFile "sorted.txt" (unwords (map show sorted))

    let binaryBase = map (\x -> toBase x 2) ls

    let res1 = zipWith (\e b -> show e ++ "\t -> \t" ++ b) ls binaryBase
    
    writeFile "binaryBase.txt" (unlines res1)

    let base16 = map (\x -> toBase x 16) ls
    let base32 = map (\x -> toBase x 32 ) ls

    let res2 = zip4 ls binaryBase base16 base32

    let ki = map (\(e, r2, r16, r32) -> show e ++ " " ++ r2 ++ " " ++ r16 ++ " " ++ r32) res2
    writeFile "bases.txt" (unlines ki)

    let primOsztok = map primeFactors ls
    let res3 = zip ls primOsztok

    let ki2 = map (\(e, ls) -> show e ++ "\t -> \t" ++ show ls) res3
    
    writeFile "primosztok.txt" (unlines ki2)

    putStrLn "VEGE"

-- III

isHamming :: Integer -> Bool
isHamming 0 = False
isHamming 1 = True
isHamming n
    | n `mod` 2 == 0 = isHamming (n `div` 2)
    | n `mod` 3 == 0 = isHamming (n `div` 3)
    | n `mod` 5 == 0 = isHamming (n `div` 5)
    | otherwise      = False


main3 = do
    let a = 10
    let b = 300
    let ls = [x | x <- [a..b], isHamming x == True]
    let temp = map (\x -> show x) ls
    writeFile "hamming.txt" (unlines temp)

    putStrLn "Vege"

luckyNumbers :: [Int]
luckyNumbers = sieve [3,5..]
    where
        sieve (x: xs) = x : sieve (removeEvery x xs)
        removeEvery n xs = map snd . filter ((/=0) . (`mod` n) . fst) $ zip [1..] xs

main4 = do
    let ls = takeWhile (< 1000) luckyNumbers
        result = unlines $ map show ls
    
    writeFile "lucky.txt" result

-- IV

--1
binKulonbozoPos :: FilePath -> FilePath -> IO [Int]
binKulonbozoPos fajl1 fajl2 = do
  b1 <- BS.readFile fajl1
  b2 <- BS.readFile fajl2
  return (kulonbozoek (BS.unpack b1) (BS.unpack b2) 0)
  where
    kulonbozoek [] [] _ = []
    kulonbozoek (x : xs) (y : ys) i
      | x /= y = i : maradek
      | otherwise = maradek
      where
        maradek = kulonbozoek xs ys (i + 1)

    -- kulonbozo hossz
    kulonbozoek [] ys i = [i .. i + length ys - 1]
    kulonbozoek xs [] i = [i .. i + length xs - 1]

--2
tartBajtszekvencia fajl bsz = do
  let bszFormaz = BC.pack bsz
  content <- BS.readFile fajl
  return (bszFormaz `BS.isInfixOf` content)

--3
fajlmeret :: IO ()
fajlmeret = do
  putStrLn "fajl neve:"
  path <- getLine
  handle <- openFile path ReadMode
  size <- hFileSize handle
  hClose handle
  putStrLn ("fajlmeret: " ++ show size ++ " bajt")

--4
readFiles = do
    line <- getLine
    if null line then
        return []
    else do
        rest <- readFiles
        return (line : rest)
    
printPair (f, s) = putStrLn (f ++ " " ++ show s ++ " bytes")

rendezettBinFajlok2 = do
    files <- readFiles

    pairs <- forM files $ \f -> do
        size <- getFileSize f 
        return (f, size)

    let sorted = sortOn snd pairs 
    mapM_ printPair sorted

--5
masolFajlok = do
  putStrLn "Add meg a fajlneveket (ures sor = vege):"
  --   files <- readFiles
  let files = ["f1.jpg"]
  eredmenyek <- forM files $ \src -> do
    content <- BS.readFile src
    let dst = takeWhile (/= '.') src ++ "_copy" ++ dropWhile (/= '.') src
    -- let dst = "07.labor/noveny_cp.jpg"
    BS.writeFile dst content
    return True
  if and eredmenyek
    then putStrLn "Masolas kesz!"
    else putStrLn "Nem sikerult!"