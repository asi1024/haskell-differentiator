import Text.ParserCombinators.Parsec

import Parser

main :: IO ()
main = do
  str <- getLine
  case parse expression "(stdin)" str of
    Left e  -> print e
    Right r -> print . show $ r
