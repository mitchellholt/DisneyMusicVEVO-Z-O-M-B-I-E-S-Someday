module ParseFile where

import Control.Applicative
import ParseLib
import Statement
import Proof
import ParseStatement
import ParseProof
import Rule


type Section = Either Theorem Definition
type Theorem = (String, Stmt (Expr String), Pf (Expr String))
type Definition = (String, [Expr String], [Expr String], Stmt (Expr String))


language :: Parser [Section]
language = do
    do
        sec <- section
        -- is there a semicolon here?
        rest <- language
        return (sec ++ rest)
    <|> section

section :: Parser [Section]
section = do
    do
        _ <- token "("
        _ <- token "def."
        name <- word
        _ <- token "("
        vars <- list
        _ <- token ")"
        _ <- token "["
        extras <- list
        _ <- token "]"
        _ <- token ")"
        _ <- token "["
        p <- statement
        _ <- token "]"
        sec <- section
        return ((Right (name, Var <$> vars, Var <$> extras, p)) : sec)
    <|> thm

thm :: Parser [Section]
thm = do
    do
        _ <- token "("
        _ <- token "thm."
        name <- word
        _ <- token ")"
        _ <- token "["
        p <- statement
        _ <- token "]"
        pf <- proof
        _ <- token "QED"
        sec <- section
        return ((Left (name, p, giveRules defaultRules pf)) : sec)
    <|> return []
