import Text.ParserCombinators.Parsec

conchFile = endBy statement (char '\n')
statement = many (satisfy 

parseConch :: String -> Either ParseError [String]
parseConch input = parse conchFile "(error)" input

{-main = do c <- getContents
          case parse conchFile "(stdin)" c of
               Left e -> do putStrLn "Error parsing input:"
                            print e
               Right r -> mapM_ print r-}