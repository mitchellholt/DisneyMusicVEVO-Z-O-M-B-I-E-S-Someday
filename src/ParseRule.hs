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
    let f = \x -> if x == p then q else x
    let fb = \x -> if x == q then p else x
    return (f, fb, name)
