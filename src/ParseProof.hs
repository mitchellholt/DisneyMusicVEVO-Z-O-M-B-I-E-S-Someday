module ParseProof where

import Control.Applicative
import Rule
import ParseRule
import ParseLib
import Proof
import Statement

{-
PROOF =
EXPAND = STATEMENT -> { PROOF } PROOF
CASE = STATEMENT : Case ( RULE ) [ PROOF ] else [ PROOF ] PROOF
APPLY = STATEMENT : ( RULE ) <- PROOF
TRUTH = -> STATEMENT ;
-}

proof :: Parser (Pf (Stmt (Expr String)))
proof = undefined
