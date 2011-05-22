import Text.ParserCombinators.Parsec

-- CSV is 0 or more lines, each terminated by eol
csvFile :: GenParser Char st [[String]]
csvFile = do result <- many line
             eof
             return result

-- Each line has 1 or more cells, comma-delimited
line :: GenParser Char st [String]
line = do result <- cells
          eol
          return result

-- Build list of cells. Tru to parse the first cell, then figure out what ends the cell
cells :: GenParser Char st [String]
cells = do first <- cellContent
           next <- remainingCells
           return (first : next)

-- The cell either ends with a comma (1 or more cells follow) or not, this is eol
remainingCells :: GenParser Char st [String]
remainingCells = (char ',' >> cells) -- found comma? more cells
                 <|> (return [])     -- no comma? no more cells

-- Each cell has 0 or more characters, which are not ',' or eol
cellContent :: GenParser Char st String
cellContent = many (noneOf ",\n")

-- The EOL char is \n
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input