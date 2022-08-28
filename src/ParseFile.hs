module ParseFile where

import Control.Applicative
import ParseLib
import Statement
import Proof
import ParseStatement
import ParseProof
import Rule


data Err a = E | J a
type Section = Either Theorem Definition
type Theorem = (String, Stmt (Expr String), Pf (Expr String))
type Definition = (String, [Expr String], [Expr String], Stmt (Expr String))


language :: Parser [Section]
language = do
    do
        sec <- section
        case sec of
            E -> return []
            J s -> do
                do
                    rest <- language
                    return (s : rest)
                <|> return [s]
    <|> return []
    

section :: Parser (Err Section)
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
        return (J (Right (name, Var <$> vars, Var <$> extras, p)))
    <|> thm

thm :: Parser (Err Section)
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
        return (J (Left (name, p, giveRules defaultRules pf)))
    <|> return E
