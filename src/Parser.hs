module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Syntax

expression :: Parser Expr
expression =  buildExpressionParser exprSpliter factor
          <?> "polynomial"

neg :: Expr -> Expr
neg p   = Mult (Number $ -1) p

sub :: Expr -> Expr -> Expr
sub l r = Plus l (neg r)

exprSpliter :: [[Operator Char () Expr]]
exprSpliter = [[op_prefix "+" id, op_prefix "-" neg],
               [op_infix "^" Power AssocRight],
               [op_infix "*" Mult AssocLeft],
               [op_infix "+" Plus AssocLeft, op_infix "-" sub AssocLeft]]
  where op_infix s f assoc = Infix (string s >> return f) assoc
        op_prefix s f = Prefix (string s >> return f)

number :: Parser Int
number = many1 digit >>= return . read

factor :: Parser Expr
factor = try (do coeff <- number
                 fact  <- primitiveFactor
                 return $ Mult (Number coeff) fact)
          <|> primitiveFactor
          <?> "error"

primitiveFactor :: Parser Expr
primitiveFactor =  try (number >>= return . Number)
                <|> (lower >>= return . Variable)
                <|> between (char '(') (char ')') expression
                <?> "error"
