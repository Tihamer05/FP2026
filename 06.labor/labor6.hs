import Data.List
import GHC.Base (VecElem(Int16ElemRep))
-- I 

main1 :: IO ()
main1 = do
    x1 <- readLn :: IO Int
    x2 <- readLn :: IO Int
    putStrLn ("x1=" ++ show x1 ++ ", x2=" ++ show x2)

main1_2 :: IO ()
main1_2 = do
    x1 <- getLine
    x2 <- getLine
    putStrLn ("x1=" ++ show (read x1 :: Int) ++ ", x2=" ++ show (read x2 :: Int))

-- a két szám közötti számok összegét
sumInterval :: (Num a, Enum a) => a -> a -> a
sumInterval left right = sum [left..right]

sumMain :: IO()
sumMain = do
    putStr "start="
    a <- readLn :: IO Int
    putStr "end="
    b <- readLn :: IO Int
    let osszeg = if a < b then sumInterval a b else sumInterval b a
    putStrLn ("Osszeg: " ++ show osszeg)

-- a két szám közötti prímszámok összegét
osztok :: Integral a => a -> [a]
osztok x = [i | i <- [1..x],  mod x i == 0]

primszam x = [1, x] == osztok x

primszamOsszeg :: Integral c => [c] -> c
primszamOsszeg ls = sum . filter primszam $ ls

primMain = do
    putStr "start="
    a <- readLn :: IO Int
    putStr "end="
    b <- readLn :: IO Int
    let sum = primszamOsszeg [a..b]
    putStrLn ("Sum: " ++ show sum)

--a két szám közötti azon számokat, amelyeknek legtöbb valódi osztója van.

valodiOsztok x = [i | i <- [2 .. div x 2], mod x i == 0, i /= x]

legtobbVO ls = filter (\(szam, vo) -> vo == maxVO) ls2 
    where
        ls2 = [(szam, length $ valodiOsztok szam) | szam <- ls]
        maxVO = maximum $ map snd ls2

voMain :: IO()
voMain = do
    putStr "start="
    a <- readLn :: IO Int
    putStr "end="
    b <- readLn :: IO Int
    let ls = [a..b]
    let voLs = legtobbVO ls
    putStrLn (show (snd . head $ voLs))
    print(map fst voLs)

-- II. Írjunk egy-egy Haskell függvényt, amely beolvassa a billentyűzetről az n természetes számot és kiírja a képernyőre

-- n-ig a Fibonacci számok listáját (n>50), úgy hogy a számok közé szóközt ír
fibo n = dropWhile ( < 50) $ takeWhile (< n) $ fiboSg 0 1 0
    where 
        fiboSg a b res = res : fiboSg b res (res + b)

fiboMain :: IO ()
fiboMain = do
    putStr "n="
    n <- readLn :: IO Int
    let fiboLs = fibo n
    mapM_ (\x -> putStr (show x ++ " ")) fiboLs
    putStrLn $ intercalate ", " $ map show  fiboLs

-- n-ig a prímszámok listáját, úgy hogy a számok közé szóközt ír

primMain_1 = do
    putStr "n="
    n <- readLn :: IO Int
    let primLs = [x | x <- [2..n], primszam x]
    putStrLn $ intercalate ", " $ map show primLs