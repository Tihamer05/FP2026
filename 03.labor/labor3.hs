-- I
--Atlag

atlag :: (Floating a) => [a] -> a
atlag ls = (sum ls) / fromIntegral (length ls)

-- II

-- meghatározza egy lista elemszámát, 2 módszerrel (myLength)

myLength1 :: (Floating a) => [a] -> Int
myLenght1 [] = 0
myLength1 (_:xs) = 1 + myLength1 xs

--myLength2 :: (Floating a) => [a] -> Int 

-- összeszorozza a lista elemeit, 2 módszerrel (myProduct)


myProduct1 :: Num a => [a] -> a
myProduct1 [] = 1
myProduct1 (x:xs) = x * myProduct1 xs

