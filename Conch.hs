module Conch where

import Text.ParserCombinators.Parsec
import System

assignment :: Parser String
assignment = do many1 (noneOf "=")
                many (char ' ')
                (char '=')
                many (char ' ')
                many1 (noneOf "=")


