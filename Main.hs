
import Differentiate (expression, differentiate)
import Polynomial (removeRedundant, printPoly)
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  input <- liftM (filter (/=' ')) getLine
  case parse expression "(stdin)" input of
    Left e  -> print e
    Right r -> print . printPoly . removeRedundant . differentiate $ r
