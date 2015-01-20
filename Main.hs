{-# LANGUAGE OverloadedStrings #-}

import Data.Text(pack, unpack, replace)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Monad(liftM)

data Polynomial = Number Int
                | Variable Int Char Int
                | Power Polynomial Int
                | Plus Polynomial Polynomial
                | Mult Polynomial Polynomial
                  deriving(Eq, Show)

main :: IO ()
main = do
  input <- liftM (filter (/= ' ')) getLine
  case parse polynomial "(stdin)" input of
    Left e  -> print e
    Right r -> print . convert . simplify . differentiate $ r

differentiate :: Polynomial -> Polynomial
differentiate (Number num) = Number 0
differentiate (Plus l r)   = Plus (differentiate l) (differentiate r)
differentiate (Mult l r)   = Plus (Mult (differentiate l) r) (Mult l (differentiate r))
differentiate (Variable coeff var power)
    | power == 1 = Number coeff
    | otherwise  = Variable (coeff * power) var (pred power)
differentiate (Power polyn power)
    | power == 1 = p'
    | power == 2 = Mult (Number power) (Mult p' polyn)
    | otherwise  = Mult (Number power) (Mult p' (Power polyn (pred power)))
    where p' = differentiate polyn

simplify :: Polynomial -> Polynomial
simplify (Plus l r)
    | l' == Number 0 = r'
    | r' == Number 0 = l'
    | otherwise      = Plus l' r'
    where l' = simplify l
          r' = simplify r
simplify (Mult l r)
    | l' == Number 0 = l'
    | l' == Number 1 = r'
    | r' == Number 0 = r'
    | r' == Number 1 = l'
    | otherwise      = Mult l' r'
    where l' = simplify l
          r' = simplify r                       
simplify p = p

convert :: Polynomial -> String
convert (Number n)    = show n
convert (Power pol p) = (convert pol) ++ "^" ++ show p
convert (Plus l r)    = (convert l) ++ "+" ++ (convert r)
convert (Mult l r)    = (putParen $ convert l) ++ "*" ++ (putParen $ convert r)
convert (Variable c v n)
    | c > 0     = s
    | otherwise = putParen s
    where s = (show c) ++ [v] ++ "^" ++(show n)

putParen s = "(" ++ s ++ ")"

polynomial =  buildExpressionParser table factor
         <?> "polynomial"

table = [[op "*" Mult AssocLeft]
        ,[op "+" Plus AssocLeft]
        ]
    where
      op s f assoc
          = Infix (string s >> return f) assoc

factor =  try signedFactor
      <|> term
      <?> "simple expression"

signedFactor = do
  coeff <- signedNum
  polyn <- between (char '(') (char ')')  polynomial
  power <- powerNum
  return $ Mult (Number coeff) (Power polyn power)

term = try variable
     <|> do
       num <- signedNum
       endOfTerm
       return (Number num)
     <?> "valid term"

variable = do
  coeff <- signedNum
  var   <- letter
  power <- powerNum
  return (Variable coeff var power)

signedNum = do
  sig <- option ' ' (char '-')
  num <- option "1" (many1 digit)
  return $ read (sig:num)

powerNum =   (char '^' >> signedNum)
         <|> (endOfTerm >> return 1)
         <?> "valid term"

endOfTerm = (lookAhead (oneOf "*+)") >> return ()) <|> eof
