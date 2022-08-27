module ParseRule where

import Rule
import Control.Applicative
import ParseLib
import Statement
import ParseStatement


-- This is INCREDIBLY sketchy
newrule :: Parser Rule
newrule = do
    p <- statement
    _ <- token "~"
    q <- statement
    _ <- token ","
    name <- word
    let f = \x -> if show x == show p then q else x
    return (f, id, name)
