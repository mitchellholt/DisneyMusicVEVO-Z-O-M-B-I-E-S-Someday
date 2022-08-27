module Proof where

import Statement
import Rule

type Env a = (Stmt a, [Rule a])

data Pf a = Expand (Env a) (Pf a) (Pf a) 
    | Case (Env a) (Rule a) (Pf a) (Pf a) (Pf a)
    | Apply (Env a) (Rule a) (Pf a) 
    | Truth (Env a)

instance Show a => Show (Pf a) where
    show (Expand (s, _) a b) = (show s) ++ " {\n" ++ (show a) ++ "\n} " ++ (show b) ++ "\n"
    show (Case (s, _) r t f n) = (show s) ++ "case()<" ++ (show t) ++ "><" ++ (show f) ++ ">" ++ (show n)
    show (Apply (s, _) r t) = "(" ++ (show s) ++ ") -> (" ++ (show t) ++ ")"
    show (Truth (s, _)) = show s