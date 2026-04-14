-- III.

atlagTU ls = [(nev, atlag jegyek) | (nev, jegyek) <- ls]
    where
        atlag ls2 = sum ls2 / fromIntegral (length ls2)

main = do 
    let lsNevjegy = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
    mapM_ (\(nev, atlagjegyek) -> putStrLn (nev ++ " " ++ show atlagjegyek)) (atlagTU lsNevjegy)