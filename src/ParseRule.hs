{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module ParseRule where

import Statement
import ParseLib
import Control.Applicative
import ParseExpression
import ParseStatement
import Proof
import Rule

rule :: Parser (Rule)
proof = do
    do
        s1 <- statement
        _ <- token "~"
        s2 <- statement
        _ <- token ","
        w <- expression
        return (createRule s1 s2 w)
--     <|> truth


createRule :: (Stmt a) -> (Stmt a) -> String -> (Rule a)


-- truth :: Parser (Pf (Expr String))
-- truth = do
--     do
--         p <- statement
--         return (Truth (p, rulesBase))
--     -- <|> exists