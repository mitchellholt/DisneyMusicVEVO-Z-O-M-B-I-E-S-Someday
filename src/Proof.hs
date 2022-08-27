module Proof where

import Statement
import Rule

type Env a = (Stmt a, [Rule a])

data Pf a = Expand (Env a) (Pf a) | Case (Env a) (Rule a) (Pf a) (Pf a)
    | Truth (Env a)
