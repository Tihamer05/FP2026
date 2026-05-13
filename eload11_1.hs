myReadFile :: FilePath -> IO [Alkalmazott]
myReadFile nev = do
    temp <- readFile nev
    let ls = map auxF $ lines temp
    return ls
        where
            auxF ls = Alkalmazott nevA jovA
                where
                [nevA, tJovA] = words ls
                jovA = (read :: String -> [FJ.Jovedelem]) tJovA
