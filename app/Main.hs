module Main where

-- interpereter structure
-- lexical analysis -> syntactical analysis -> ??? -> evaluation -> out

data Expr
  = Number Int
  | Add Expr Expr
  | Sub Expr Expr
  -- | Var Ident
  -- | Let Ident Expr Expr

data Value = NumVal Int

eval :: Expr -> Value
eval (Number x) = NumVal x
eval (Add x y) = let NumVal x' = eval x in
                 let NumVal y' = eval y in
                 NumVal(x' + y')
eval (Sub x y) = let NumVal x' = eval x in
                 let NumVal y' = eval y in
                 NumVal(x' - y')



main :: IO ()
main = putStrLn "Hello, Haskell!"
