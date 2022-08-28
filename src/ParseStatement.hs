{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module ParseStatement where

import Statement
import ParseLib
import Control.Applicative
import ParseExpression


statement :: Parser (Stmt (Expr String))
statement = do
    do
        p <- predicate
        _ <- token "or"
        q <- statement
        return (Or p q)
    <|> predicate

predicate :: Parser (Stmt (Expr String))
predicate = do
    do
        _ <- token "!"
        p <- statement
        return (Not p)
    <|> exists

exists :: Parser (Stmt (Expr String))
exists = do
    do
        _ <- token "E"
        expr <- expression
        case expr of
            (Var x) -> do
                _ <- token ":"
                p <- statement
                return (Exists (Var x) p)
            _ -> empty
    <|> forall

forall :: Parser (Stmt (Expr String))
forall = do
    do
        _ <- token "A"
        expr <- expression
        case expr of
            (Var x) -> do
                _ <- token ":"
                p <- statement
                return (Forall (Var x) p)
            _ -> empty
    <|> stmtEq

stmtEq :: Parser (Stmt (Expr String))
stmtEq = do
    do
        x <- expression
        _ <- token "="
        y <- expression
        return (Eq x y)
    <|> stmtParens

stmtParens :: Parser (Stmt (Expr String))
stmtParens = do
    do
        _ <- token "("
        stmt <- statement
        _ <- token ")"
        return stmt
    <|> taut

taut :: Parser (Stmt (Expr String))
taut = do
    do
        _ <- token "Taut"
        return Taut
