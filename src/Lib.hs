module Lib
  ( Expr (..),
    vars,
    Vals,
    vals,
    eval,
    exprs,
  )
where

import Data.List
import Data.Maybe (fromJust)

data Expr
  = Var Char
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  | Cond Expr Expr
  | Bicond Expr Expr

paren e = "(" ++ e ++ ")"

instance Show Expr where
  show (Var c) = [c]
  show (Not e) = '¬' : show e
  show (And l r) = paren $ show l ++ " ∧ " ++ show r
  show (Or l r) = paren $ show l ++ " ∨ " ++ show r
  show (Cond l r) = paren $ show l ++ " → " ++ show r
  show (Bicond l r) = paren $ show l ++ " ↔ " ++ show r

vars :: Expr -> [Char]
vars (Var c) = [c]
vars (Not e) = vars e
vars (And l r) = vars l ++ vars r
vars (Or l r) = vars l ++ vars r
vars (Cond l r) = vars l ++ vars r
vars (Bicond l r) = vars l ++ vars r

type Vals = [(Char, Bool)]

val :: Char -> Vals -> Bool
val c = snd . fromJust . find ((== c) . fst)

vals :: [Char] -> [Vals]
vals [] = [[]]
vals (c : cs) = [(c, b) : rest | rest <- vals cs, b <- [True, False]]

-- | bubbles up internal expressions
exprs :: Expr -> [Expr]
exprs (Var c) = [] -- don't keep plain vars; covered in vars
exprs (Not e) = Not e : exprs e
exprs (And l r) = And l r : exprs l ++ exprs r
exprs (Or l r) = Or l r : exprs l ++ exprs r
exprs (Cond l r) = Cond l r : exprs l ++ exprs r
exprs (Bicond l r) = Bicond l r : exprs l ++ exprs r

eval :: Expr -> Vals -> Bool
eval (Var c) v = val c v
eval (Not e) v = not . eval e $ v
eval (And l r) v = (eval l v) && (eval r v)
eval (Or l r) v = (eval l v) || (eval r v)
eval (Cond l r) v = (not $ eval l v) || (eval r v)
eval (Bicond l r) v = (eval l v) == (eval r v)
