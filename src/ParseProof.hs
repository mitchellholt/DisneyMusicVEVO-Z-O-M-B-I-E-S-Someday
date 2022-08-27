{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module ParseProof where

import Statement
import ParseLib
import Control.Applicative
import ParseExpression
import ParseStatement
import Proof
import Rule

theorem :: Parser (Pf (Expr String))
theorem = do
    do
        _ <- token "["
        g <- statement
        _ <- token "]"
        _ <- token "["
        c <- proof
        _ <- token "["
        n <- proof
        return (Expand (g, rulesBase) c n)
    <|> truth

expand :: Pa


    -- <|> exists