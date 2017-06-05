import Language.MiniStg.Parser (parseStg)

main = do
        s <- getLine
        let t = parseStg s
        putStrLn . show $ t
