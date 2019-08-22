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
data Basetype = DInt | DChar | DFloat
                deriving Show
type Hasktype = [Basetype]

intbase     :: Parser Basetype
intbase     =  do string "Int"
                  return DInt

charbase    :: Parser Basetype
charbase    =  do string "Char"
                  return DChar

floatbase   :: Parser Basetype
floatbase   =  do string "Float"
                  return DFloat

parsebase   :: Parser Basetype
parsebase   =  do intbase
                <|> (do charbase
                      <|> floatbase)

typeparser  :: Parser Hasktype                                          -- para eliminar espacios blancos
typeparser  =  do x <- sepBy (token parsebase) (string "->")            -- x <- sepBy parsebase (string " -> ")
                  return x

-- 7) 
data Haskelltype = DDInt | DDChar | Fun Haskelltype Haskelltype
                   deriving Show

intbase'    :: Parser Haskelltype
intbase'    =  do string "Int"
                  return DDInt

charbase'   :: Parser Haskelltype
charbase'   =  do string "Char"
                  return DDChar

parsebase'  :: Parser Haskelltype
parsebase'  =  do intbase'
                <|> charbase'

typepar     :: Parser Haskelltype           -- para cantidad arbitraria de espacios (en cualquier lugar):
typepar     =  do t1 <- parsebase'          -- t1 <- token parsebase'
                  t2 <- typepar'
                  return (Fun t1 t2)

typepar'    :: Parser Haskelltype
typepar'    =  do string " -> "             -- string "->"
                  t2 <- parsebase'          -- t2 <- token parsebase'
                  (do t3  <- typepar'
                      return (Fun t2 t3)
                    <|> return t2)
                  
                  













