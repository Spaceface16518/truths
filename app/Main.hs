module Main where

import Control.Monad
import Data.List
import Lib
import Parser
import Text.SimpleTableGenerator
import Util

main :: IO ()
-- main = do i <- getContents
-- _ <- mapM putStr . lines $ i
-- return ()
main = putStrLn $ process ""

reverse' :: [Expr] -> [Expr]
reverse' l = rev l [] []
  where
    rev [] v a = reverse v ++ a
    rev ((Var c) : xs) v a = rev xs (Var c : v) a
    rev (x : xs) v a = rev xs v (x : a)

process :: String -> String
process input =
  let e = And (Not (Var 'p')) (Var 'q')
      base = vars e -- base variables
      es = reverse' $ exprs e -- all expressions, including intermediate
      v = vals base -- base truth table
      tt = map (\e' -> map (eval e') v) es
      headers = map show es
      columns = transpose $ map2d show tt
   in makeDefaultSimpleTable $ headers : columns
