import Data.List
import Data.Ord

type Nev = String
type Kod = Int
type Jegyek = [Double]

data DiakH = DiakH {
    dNev :: Nev,
    dEv :: Kod,
    dJegyek :: Jegyek
    } deriving (Show)

data DiakM = DiakM Nev Kod Jegyek
    deriving Show

hasonlit :: DiakH -> DiakH -> DiakH
hasonlit d1 d2
    = if atlag (dJegyek d1) > atlag (dJegyek d2) then d1 else d2

hasonlit1 :: DiakH -> DiakH -> DiakH
hasonlit1 h1@(DiakH a1 a2 a3) h2@(DiakH b1 b2 b3)
    = if atlag a3 > atlag b3 then h1 else h2

atlag :: (Fractional a, Foldable t) => t a -> a
atlag ls = sum ls / fromIntegral (length ls)

data Hallgato = Hallgato{
    hNev :: String,
    hJegy :: Double,
    hEv :: Int
} deriving(Show)

hallgatoL = [Hallgato "Sari" 8.75 1, Hallgato "Mari" 4.25 1,
            Hallgato "Feri" 3.5 2, Hallgato "Zsuzsi" 10.0 2,
            Hallgato "Laci" 8.5 2, Hallgato "Lori" 7.5 2]

szamol :: [Hallgato] -> Int
szamol ls = length $ filter (\(Hallgato a b c) -> b > 4.5) ls


valogat :: Int -> [Hallgato] -> [Hallgato]
valogat e = foldr op []
    where
    op :: Hallgato -> [Hallgato] -> [Hallgato]
    --op k rLs = if hEv k == e then k : rLs else rLs
    op k@(Hallgato a b c) rLs = if c == 0 then k : rLs else rLs

myShow :: Hallgato -> String
myShow k = hNev k ++ " " ++ show (hJegy k) ++ " " ++ show (hEv k)

-- main4 = do
--     putStr "ev: "
--     tStr <- getLine
--     let e = read tStr :: Int
--     --let nLs = valogat e hallgatoL
--     --let nLs = valogatLC e hallgatoL
--     --mapM_ (putStrLn . myShow) nLs

rendez :: Ord b => (a -> b) -> [a] -> [a]
rendez mezoNev ls = sortOn mezoNev ls

main5 = do
    putStr "Mi szerint szeretn;l rendezni? (nev, jegy, ev): "
    temp <- getLine
    let resL = case temp of
            "nev" -> rendez hNev hallgatoL
            "jegy" -> rendez (Down . hJegy) hallgatoL
            "ev" -> rendez hEv hallgatoL
            _ -> error "nincs ilyen mezon"
    mapM_ (putStrLn . myShow) resL


type Jovedelem = (Int, Int)
data Alkalmazott = Alkalmazott {
    alkNev :: String,
    alkJovedelem :: [Jovedelem]
} deriving (Show, Read)


mainAlkalmazott :: IO ()
mainAlkalmazott = do
    temp <- readFile "alkalmazottData.txt"
    let lsAlk = (read :: String -> [Alkalmazott]) temp
    --mapM_ print lsAlk
    putStr "ev: "
    temp <- getLine
    let ev = read temp :: Int
    foJovedelem ev lsAlk

auxEv :: Int -> [Jovedelem] -> Maybe Int
auxEv ev = foldr (op ev) Nothing
    where
    op ev (k1, k2) res =
        if ev == k1 then Just k2 else res

foJovedelem :: Int -> [Alkalmazott] -> IO()
foJovedelem ev = mapM_ (auxF ev)
    where
    auxF :: Int -> Alkalmazott -> IO()
    auxF ev k =
        case res of
            Just x -> putStrLn $ alkNev k ++ ", " ++ show x
            Nothing -> putStrLn $ alkNev k ++ ", nincs jovedelem"
        where
        res = auxEv ev (alkJovedelem k)

data Bool = False | True

type KartyaSz = String
type Tulajdonos = String
type Cim = [String]
type FelhasznaloID = Int

data BankSzamla = BankKartya KartyaSz Tulajdonos Cim
    | Keszpenz
    | Szamla FelhasznaloID
    deriving (Show, Eq)

sz1 = BankKartya "12321" "Kiss Antal" ["Mvh", "Romania"]
sz2 = Keszpenz
sz3 = Szamla 12
sz4 = BankKartya "54321" "Nagy Antal" ["Kv", "Romania"]
sz5 = BankKartya "98765" "Beres Antal" ["Mvh", "Romania"]
sz6 = Szamla 13
lsBsz = [sz1, sz2, sz3, sz4, sz5, sz6]

valogatBK :: [BankSzamla] -> ([BankSzamla], [Int], [BankSzamla])
valogatBK ls = auxV ls [] [0] []
    where
    auxV [] bLs kLs sLs = (bLs, kLs, sLs)
    auxV (k : ve) bLs kLs sLs = case k of
        BankKartya {} -> auxV ve (k : bLs) kLs sLs
        Keszpenz -> auxV ve bLs [1 + head kLs] sLs
        Szamla _ -> auxV ve bLs kLs (k : sLs)
