import Data.List

-- az első n páros szám négyzetét
parosNegyzet n = take n [x ^ 2 | x <- [2,4..]]

parosNegyzet2 :: (Floating a, Enum a) => Int -> [a]
parosNegyzet2 n = take n $ map (\i -> i ** 2 ) [2,4..]

negyzet3 :: Int -> IO ()
negyzet3 n = mapM_ (\(szam, negyzete) -> putStrLn (show szam ++ " negyzete " ++ show negyzete)) ls
  where
    ls = take n $ map (\i -> (i, i ^ 2)) [2, 4 ..]

szamokLs :: Int -> [Int]
szamokLs 1 = replicate 1 1
szamokLs n = szamokLs (n - 1) ++ replicate n n

szamokLs2 :: Int -> Int -> [Int]
szamokLs2 n i 
    | i /= n = replicate i i ++ szamokLs2 n (i + 1)
    | otherwise = replicate i i 

szamokLs3 :: Num t => Int -> Int -> t -> [t]
szamokLs3 n i j
    | i /= n = replicate i (j + 2) ++ szamokLs3 n (i + i) (j + 1)
    | otherwise  = replicate i (j + 2)

szamokLs4 n = [n,n - 1 .. 1] ++ [1 .. n]

valtakozo n = take n ls
  where
    ls = [True, False] ++ ls

valtakozo2 n = take n ls
  where
    ls = [0, 1, -1] ++ ls


osztok x = length [i | i <- [1..x], mod x i == 0]

osztok2 x = myLength [i | i <- [1 .. x], mod x i == 0]
  where
    myLength [] = 0
    myLength (_ : ls) = 1 + myLength ls

osztok3 x = foldl (\res i -> if mod x i == 0 then res + 1 else res) 0 [1 .. x]

osztok4 x = foldl (\res i -> if mod x i == 0 then res + 1 else res) 1 [1 .. div x 2]

maxParatlanOsztok n = last [i | i <- [1, 3 .. n], mod n i == 0]

maxParatlanOsztok2 n = maximum [i | i <- [1, 3 .. n], mod n i == 0]

maxParatlanOsztok3 n = maximum [i | i <- [1 .. n], mod n i == 0, odd i]

maxParatlanOsztok4 n = myMaximum [i | i <- [1, 3 .. n], mod n i == 0]
  where
    myMaximum [x] = x
    myMaximum (x1 : x2 : xs)
      | x1 > x2 = myMaximum (x1 : xs)
      | otherwise = myMaximum (x2 : xs)

maxParatlanOsztok5 n
  | odd n = n
  | otherwise = foldl (\acc x -> if mod n x == 0 then x else acc) 1 [1, 3 .. n]

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,
decP :: Integral t => t -> t -> [t]
decP x p
  | x < p = [x]
  | otherwise = decP (div x p) p ++ [mod x p]

decPSzam :: Integral a => a -> a -> Int
decPSzam x p = length $ decP x p

decpSzam3 :: (Integral a, Num b) => a -> a -> b
decpSzam3 x p = foldl (\res i -> res + 1) 0 (decP x p)

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,
decPMax :: Integral a => a -> a -> a
decPMax x p = maximum $ decP x p

-- - meghatározza az $a$ és $b$ közötti Fibonacci számokat, $a > 50$.
fibo :: (Ord p, Num p) => p -> p -> [p]
fibo a b = dropWhile (< a) $ fiboSg 0 1 0
  where
    fiboSg a1 b1 res
      | res < b = res : fiboSg b1 res (res + b1)
      | otherwise = [res]

fibo2 :: [Integer]
fibo2 = fiboSg 0 1 0
  where
    fiboSg a b res = res : fiboSg b res (b + res)

fiboAB :: Integer -> Integer -> [Integer]
fiboAB a b = dropWhile (< a) $ takeWhile (< b) fibo2

fibo1 a b = filter(\x -> x > a && x < b) (fibo2 0 1 0)
  where
    fibo2 a1 b1 res
      | res < b = res : fibo2 b1 res (res + b1)
      | otherwise = [res]

-- III

-- meghatározza egy lista pozitív elemeinek átlagát,

atlag ls = (sum ls) / fromIntegral (length ls)

positiveAvg :: (Fractional a, Ord a) => [a] -> a
positiveAvg ls = atlag [x | x <- ls, x > 0]

positiveAvg2 :: (Fractional c, Ord c) => [c] -> c
positiveAvg2 ls = atlag . filter (> 0) $ ls 

-- meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,
lsitaN :: Integral a1 => [a2] -> a1 -> [a2]
lsitaN ls n = [i | (idx, i) <- zip [1..] ls, mod idx n == 0]

listaN2 :: [a] -> Int -> Int -> [a]
listaN2 ls n i 
  | i - 1 >= length ls = []
  | mod i n == 0 = ls !! (i - 1) : listaN2 ls n (i + 1)
  | otherwise = listaN2 ls n (i + 1)

lsitaN3 ls n = filter (\x -> mod (fst x) n == 0) (zip [1..] ls)

tukor :: (Eq a1, Num a1, Foldable t) => a1 -> t a2 -> [a2]
tukor 2 ls = foldl (\res x -> x : res) [] ls

maxElemPoz ls = [idx | (idx, i) <- zip [1..] ls, i == myMax]
  where
    myMax = maximum ls

maxElemPoz2 (x:ls) = foldl aux (x, [0]) (zip ls [1..])
  where 
    aux (currentMax, positions) (el, i)
      | el > currentMax  = (el, [i])
      | el == currentMax = (currentMax, positions ++ [i])
      | otherwise = (currentMax, positions)

-- meghatározza egy lista leggyakrabban előforduló elemét.
elof ls = map (\x -> (x, length x)) $ (group . sort) ls

elof1 ls = maxElofElem
  where 
    maxElofSzam = maximum $ map length $ (group . sort) ls
    ls2 = map (\x -> (head x, length x)) $ (group . sort) ls
    maxElofElem = head $ fst $ head $ filter (\x -> snd x == maxElofSzam) ls2