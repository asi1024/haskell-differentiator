
module Differentiate
    (
     Tree(..),
     expression,
     -- differentiate,
    ) where

import Data.Text(pack, unpack, replace)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Monad (liftM)

data Tree = Number Int
          | Variable Char
          | Power Tree Int
          | Plus Tree Tree
          | Mult Tree Tree
            deriving(Eq, Show)

-- differentiate :: Tree -> Tree
-- differentiate (Number num) = Number 0
-- differentiate (Plus l r)   = Plus (differentiate l) (differentiate r)
-- differentiate (Mult l r)   = Plus (Mult (differentiate l) r) (Mult l (differentiate r))
-- differentiate (Variable coeff var power)
--     | power == 1 = Number coeff
--     | otherwise  = Variable (coeff * power) var (pred power)
-- differentiate (Power n polyn power)
--     | power == 1 = Mult (Number n) p'
--     | power == 2 = Mult (Number n') (Mult p' polyn)
--     | otherwise  = Mult p' (Power n' polyn (pred power))
--     where p' = differentiate polyn
--           n' = n * power

expression =  buildExpressionParser exprSpliter factor
          <?> "polynomial"

neg p   = Mult (Number $ -1) p
sub l r = Plus l (neg r)

exprSpliter = [[op "*" Mult AssocLeft]
              ,[op "+" Plus AssocLeft, op "-" sub AssocLeft]
              ]
    where
      op s f assoc
          = Infix (string s >> return f) assoc

factor =  try (do
                string "-"
                fact <- absFactor
                return $ neg fact)
      <|> try (do
                string "+"
                absFactor)
      <|> absFactor
      <?> "factor"

number = many1 digit >>= return . read

absFactor =  try (do
                   coeff <- number
                   fact  <- simpleFactor
                   return $ Mult (Number coeff) fact)
         <|> simpleFactor
         <?> "error"

unaryOp =  try (do
                  string "-"
                  num <- number
                  return $ negate num)
        <|> try (do
                  string "+"
                  number)
        <|> number

simpleFactor =  try (do
                      exp <- primitiveFactor
                      string "^"
                      num <- unaryOp
                      return $ Power exp num )
            <|> primitiveFactor
            <?> "error"

primitiveFactor =  try (number >>= return . Number)
               <|> (lower >>= return . Variable)
               <|> between (char '(') (char ')') expression
               <?> "error"
