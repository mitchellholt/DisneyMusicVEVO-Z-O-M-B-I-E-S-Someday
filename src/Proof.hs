module Proof where

import Statement
import Rule

type Env a = (Stmt a, [Rule])

data Pf a = Expand (Env a) (Pf a) (Pf a) 
    | Case (Env a) Rule (Pf a) (Pf a) (Pf a)
    | Apply (Env a) Rule (Pf a) 
    | Truth (Env a)

mapToEnv :: (Env a -> Env b) -> Pf a -> Pf b
mapToEnv f (Expand env p1 p2) = Expand (f env) (mapToEnv f p1) (mapToEnv f p2)
mapToEnv f (Case env rule p1 p2 p3) = Case (f env) rule (mapToEnv f p1) (mapToEnv f p2) (mapToEnv f p3) 
mapToEnv f (Apply env rule pf) = Apply (f env) rule (mapToEnv f pf)
mapToEnv f (Truth env) = Truth (f env)


instance Show a => Show (Pf a) where
    show (Expand (s, _) a b) = (show s) ++ " {\n" ++ (show a) ++ "\n} " ++ (show b) ++ "\n"
    show (Case (s, _) r t f n) = (show s) ++ "case()<" ++ (show t) ++ "><" ++ (show f) ++ ">" ++ (show n)
    show (Apply (s, _) r t) = "(" ++ (show s) ++ ") -> (" ++ (show t) ++ ")"
    show (Truth (s, _)) = show s


giveRules :: [Rule] -> Pf a -> Pf a
giveRules rules = mapToEnv (addToEnv rules)
    where
        addToEnv :: [Rule] -> Env a -> Env a
        addToEnv r (p, xs) = (p, xs ++ r)

