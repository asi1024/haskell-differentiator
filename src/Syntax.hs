module Syntax where

data Expr = Number Int
          | Variable Char
          | Power Expr Expr
          | Plus Expr Expr
          | Mult Expr Expr
            deriving(Eq, Show)
