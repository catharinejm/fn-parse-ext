import Text.ParserCombinators.Parsec
import System

conchFile = endBy statement (char '\n')
statement = many anyChar

parseConch :: [String] -> Either ParseError String
parseConch [] = []
parseConch input = (parse conchFile "(error)" input) : parseConch (tail input)

main :: IO ()
main = do args <- getArgs
          putStrLn (parseConch args)