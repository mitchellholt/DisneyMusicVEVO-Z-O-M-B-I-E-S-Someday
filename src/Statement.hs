module Statement where

import Expression

-- Type used on statement should be epression
data Stmt a = EQ a a | Or (Stmt a) (Stmt a) | And (Stmt a) (Stmt a)
    | Not (Stmt a) (Stmt a) | Exists a (Stmt a) | Forall a (Stmt a)
    deriving (Eq, Show)
