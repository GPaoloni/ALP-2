-- Pràctica 1 ALP - Parsers
module Practica1 where 

import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)


-- 2)
expr        :: Parser Int
expr        =  do t <- term 
                  (do char '+'
                      e <- expr 
                      return (t + e)
                    <|> (do char '-'
                            e <- expr
                            return (t - e)
                      <|> return t))

term        :: Parser Int
term        =  do f <- factor
                  (do char '*'
                      t <- term
                      return (f * t)
                    <|> (do char '/'
                            t <- term
                            return (f `div` t)
                      <|> return f))

factor      :: Parser Int
factor      =  do d <- digit
                  return (digitToInt d)
                <|> do char '('
                       e <- expr
                       char ')'
                       return e

eval        :: String -> Int
eval xs     =  fst (head (parse expr xs))

-- 3)
paren       :: Parser a -> Parser a
paren p     =  do char '('
                  x <- p
                  char ')'
                  return x
                <|> p

-- 4)
data Expr   = Num Int | BinOp Op Expr Expr
              deriving Show
data Op     = Add | Mul | Min | Div
              deriving Show

expr'       :: Parser Expr
expr'       =  do t <- term' 
                  (do char '+'
                      e <- expr' 
                      return (BinOp Add t e)
                    <|> (do char '-'
                            e <- expr'
                            return (BinOp Min t e)
                      <|> return t))

term'       :: Parser Expr
term'       =  do f <- factor'
                  (do char '*'
                      t <- term'
                      return (BinOp Mul f t)
                    <|> (do char '/'
                            t <- term'
                            return (BinOp Div f t)
                      <|> return f))

factor'     :: Parser Expr
factor'     =  do d <- digit
                  return (Num $ digitToInt d)
                <|> do char '('
                       e <- expr'
                       char ')'
                       return e

eval'       :: String -> Expr
eval' xs    =  fst (head (parse expr' xs))

-- 5)
data Elem   = N Int | C Char
              deriving Show

element     :: Parser Elem
element     = do n <- digit
                 return (N $ digitToInt n)
               <|> do c <- letter
                      return (C c)

hetero      :: Parser [Elem]
hetero      =  do char '['
                  x <- sepBy (token element) (char ',')
                  char ']'
                  return x

-- 6) 












