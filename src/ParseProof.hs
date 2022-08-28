{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module ParseProof where

import Statement
import ParseLib
import Control.Applicative
import ParseExpression
import ParseStatement
import ParseRule
import Proof
import Rule

-- BRACKETS = [ PROOF ]
-- EXPAND =  STATEMENT -> PROOF <- PROOF
-- CASE = STATEMENT : Case ( RULE ) { PROOF } else { PROOF } <- PROOF
-- APPLY = STATEMENT : ( RULE ) <- PROOF
-- TRUTH = STATEMENT ;

proof :: Parser (Pf (Expr String))
proof = do
    do
        _ <- token "["
        p <- proof
        _ <- token "]"
        return p
    <|> expand

expand :: Parser (Pf (Expr String))
expand = do
    do
        s <- statement
        _ <- token "->"
        i <- proof
        _ <- token "<-"
        n <- proof
        return (Expand (s, []) i n)
    <|> casep

casep :: Parser (Pf (Expr String))
casep = do
    do
        s <- statement
        _ <- token ":"
        _ <- token "Case"
        _ <- token "("
        r <- newrule 
        _ <- token ")"
        _ <- token "{"
        t <- proof
        _ <- token "}"
        _ <- token "else"
        _ <- token "{"
        f <- proof
        _ <- token "}"
        _ <- token "<-"
        n <- proof
        return (Case (s, []) r (giveRules [r] t) (giveRules [r] f) n)
    <|> apply

apply :: Parser (Pf (Expr String))
apply = do
    do
        s <- statement
        _ <- token ":"
        _ <- token "("
        r <- word
        _ <- token ")"
        _ <- token "<-"
        n <- proof
        return (Apply (s, []) (id, id, r) n) -- needs to change
    <|> truth

truth :: Parser (Pf (Expr String))
truth = do
    do
        s <- statement
        _ <- token ";"
        return (Truth (s, [])) -- needs to change
    -- <|> case