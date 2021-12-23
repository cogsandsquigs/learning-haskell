module Main where

-- interpereter structure
-- lexical analysis -> syntactical analysis -> ??? -> evaluation -> out

data Expr
  = Value Int
  | Add Expr Expr
  | Sub Expr Expr
  deriving (Show)

eval :: Expr -> Int
eval (Value x) =  x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y



main :: IO ()
main = print (eval (Add (Value 10) (Sub (Value 5) (Value 20))))