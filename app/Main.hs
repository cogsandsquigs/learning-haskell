module Main where

-- interpereter structure
-- lexical analysis -> syntactical analysis -> ??? -> evaluation -> out

data Expr
  = Value Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show)

eval :: Expr -> Either String Int
eval (Value x) = Right x
eval (Add x y) = do
  x' <- eval x
  y' <- eval y
  Right (x' + y')
eval (Sub x y) = do
  x' <- eval x
  y' <- eval y
  Right (x' - y')
eval (Mul x y) = do
  x' <- eval x
  y' <- eval y
  Right (x' * y')
eval (Div x y) = do
  x' <- eval x
  y' <- eval y
  if y' == 0
    then Left "divide by zero error"
    else Right (div x' y')

main :: IO ()
main = print (eval (Div (Value 1) (Value 0)))