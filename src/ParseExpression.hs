{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module ParseExpression where

import Statement
import ParseLib
import Control.Applicative


expression :: Parser (Expr String)
expression = do
    do
        x <- term
        _ <- token "+"
        y <- expression
        return (Sum x y)
    <|> term

term :: Parser (Expr String)
term = do
    do
        x <- factor
        _ <- token "*"
        y <- term
        return (Prod x y)
    <|> successor


successor :: Parser (Expr String)
successor = do
    do
        _ <- token "s"
        _ <- token "("
        expr <- expression
        _ <- token ")"
        return (Succ expr)
    <|> zero

factor :: Parser (Expr String)
factor = do
    do
        _ <- token "("
        x <- expression
        _ <- token ")"
        return x
    <|> zero

zero :: Parser (Expr String)
zero = do
    do
        _ <- token "0"
        return Zero
    <|> var

var :: Parser (Expr String)
var = do
    x <- word
    return (Var x)
