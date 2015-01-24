
module Polynomial
    (
     printPoly,
     removeRedundant
    ) where

import qualified Differentiate as D
import Data.List
import Debug.Trace

data Polynomial = Number Int
                | Variable Int Char Int
                | Plus [Polynomial]
                | Mult [Polynomial]
                | Power Int Polynomial Int
                  deriving(Eq, Show, Ord)


removeRedundant :: D.Tree -> Polynomial
removeRedundant = fixedPointIter (sortFactor . flattenPlusMult . simplify . foldConst) . convert

fixedPointIter f x = if x == f x
                     then x
                     else fixedPointIter f (f x)

isNumber :: Polynomial -> Bool
isNumber (Number n) = True
isNumber _          = False

convert :: D.Tree -> Polynomial
convert (D.Number n) = Number n
convert (D.Variable n v p) = Variable n v p
convert (D.Power n tree p) = Power n (convert tree) p
convert (D.Plus l r) = Plus ((convert l):(convert r):[])
convert (D.Mult l r) = Mult ((convert l):(convert r):[])

flattenPlusMult :: Polynomial -> Polynomial
flattenPlusMult (Plus ps) = Plus (foldl f [] ps')
    where ps' = map flattenPlusMult ps
          f acc (Plus p) = p ++ acc 
          f acc t        = t:acc 
flattenPlusMult (Mult ps) = Mult (foldl f [] ps)
    where ps' = map flattenPlusMult ps
          f acc (Mult p) = p ++ acc 
          f acc t        = t:acc
flattenPlusMult (Power n pol p) = Power n (flattenPlusMult pol) p
flattenPlusMult p = p

foldConst :: Polynomial -> Polynomial
foldConst (Plus p) = if sum == 0
                     then Plus rest
                     else Plus $ rest ++ [Number sum]
    where p'         = map foldConst p
          (ns, rest) = partition isNumber p'
          sum        = foldl (\acc (Number n) -> acc + n) 0 ns
foldConst (Mult p)
    | prd == 0  = Number 0
    | prd == 1  = Mult rest
    | otherwise = Mult $ (Number prd):rest
    where (prd, rest) = foldCoeff $ map foldConst p

foldConst (Power n pol p) = Power n (foldConst pol) p
foldConst p = p

foldCoeff = foldBody 1 []
    where foldBody prd new [] = (prd, reverse new)
          foldBody prd new (p:ps)
              = case p of
                  Number n       -> foldBody (prd * n) new ps
                  Variable n x p -> foldBody (prd * n) ((Variable 1 x p):new) ps
                  Power n pol p  -> foldBody (prd * n) ((Power 1 pol p):new) ps
                  p              -> foldBody prd (p:new) ps

simplify :: Polynomial -> Polynomial
simplify (Power n pol p) = if p == 1
                           then Mult ((Number n):(simplify pol):[])
                           else Power n pol p
simplify (Mult p) = redundant Mult p
simplify (Plus p) = redundant Plus p
simplify p = p

redundant op p = if length p == 1
                 then simplify . head $ p
                 else op $ map simplify p

sortFactor :: Polynomial -> Polynomial
sortFactor (Mult p)        = Mult (sort . map sortFactor $ p)
sortFactor (Power n pol p) = Power n (sortFactor pol) p
sortFactor (Plus p)        = Plus (reverse . sort . map sortFactor $ p)
sortFactor p = p

printPoly :: Polynomial -> String
printPoly (Plus ps) = plusFolder ps
printPoly p         = printPoly' p

printPoly' :: Polynomial -> String
printPoly' (Number n)       = show n
printPoly' (Variable n x p) = makeTerm n [x] p
printPoly' (Power n pol p)  = makeTerm n (printPoly' pol) p
printPoly' (Plus ps)        = putParens $ plusFolder ps
printPoly' (Mult ps)        = multFolder ps

putParens s = "(" ++ s ++ ")"
makeTerm n x p = (normalizedCoeff n) ++ x ++ (normalizedPower p)

normalizedCoeff n
    | n == 1    = ""
    | n == -1   = "-"
    | otherwise = show n

normalizedPower p = if p == 1
                    then ""
                    else "^" ++ (show p)
                         
plusFolder ps = foldl plusInserter (head ps') (tail ps')
    where ps' = map printPoly' ps
          plusInserter acc x = if head x == '-'
                               then acc ++ x
                               else acc ++ "+" ++ x

multFolder ps = foldl multInserter start (tail ps')
    where ps' = map printPoly' ps
          start = if head ps' == "-1"
                  then "-"
                  else head ps'
          multInserter acc x = if head x == '-'
                               then acc ++ putParens x
                               else acc ++ x
