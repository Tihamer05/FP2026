emailFelbontasa lsE = mapM_ myPrint resL 
    where 
        myPrint (t1, t2, t3) = putStrLn $ t1 ++ " " ++ t2 ++ " " ++ t3
        resL = map fgM lsE
        fgM (t1, t2, t3) = (userN, szerverN, domainN)
            where
                userN = takeWhile (/= '@') t1
                rest = dropWhile (/= '@') t1
                szerverN = tail $ takeWhile (/= '.') rest
                domainN = tail $ dropWhile (/= '.') rest

main :: IO ()
main  = do
    let lsE = [("rosalesanthony@gmail.com", "03/31", "213130957725524"),
                ("robin18@example.net", "02/29", "570620146482"),
                ("bsullivan@example.org", "03/27", "4215057708441701869"),
                ("jameshughes@example.org", "09/27", "4782851642138996"),
                ("douglasjordan@yahoo.com", "03/27", "5289954454350249"),
                ("jwells@example.net", "06/31", "342926219737676"),
                ("spotter@gmail.com", "01/27", "4917299108623093")]

    emailFelbontasa lsE