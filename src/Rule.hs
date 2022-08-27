{-# LANGUAGE LambdaCase #-}
module Rule where

import Statement

type Rule a = (Stmt a -> Stmt a, Stmt a -> Stmt a, String)


not :: Rule (Expr a)
not = (Not, Not)


associativity :: Rule (Expr a)
associativity =
    let 
        f = \case
            (Or p (Or q r)) -> (Or (Or p q) r)
            s               -> s
        g = \case
            (Or (Or p q) r) -> (Or p (Or q r))
            s               -> s
    in
        (f, g)


-- stbs f = (Eq x y -> Eq (f x) (f y), Eq (f x) (f y) -> Eq (f x) (f y)
stbs :: (Expr a -> Expr a) -> Rule (Expr a)
stbs f =
    let
        g = \case
            (Eq x y) -> Eq (f x) (f y)
            r        -> r
            
    in
        (g, id)


commute :: Rule (Expr a)
commute =
    let
        g = \case
            (Eq x y) -> Eq y x
            (Or x y) -> Or y x
            r        -> r
    in
        (g, g)


-- A x, ~(0 = s(x))
axiom1 :: Eq a => Rule (Expr a)
axiom1 =
    let
        g = \case
            s@(Forall (Var x) (Not (Eq Zero (Succ y))))
                | y == Var x -> Taut
                | otherwise -> s
            r -> r
    in
        (g, id)


-- forall x, forall y, sx = sy => x = y
axiom2 :: Rule (Expr a)
axiom2 =
    let
        f = \case
            (Eq (Succ x) (Succ y)) -> Eq x y
            r                      -> r
        g = \case
            (Eq x y) -> Succ <$> Eq x y
            s        -> s
    in
        (f, g)


-- forall y, (y = 0) or (exists x : s(x) = y)
axiom3 :: Eq a => Rule (Expr a)
axiom3 = 
    let
        g = \case
            p@(Exists x (Eq (Succ z) y))
                | x == z    -> (Eq y Zero)
                | otherwise -> p
            -- TODO put converse here
            r -> r
    in 
        (g, g)


-- Forall x, x + 0 = x
axiom4 :: Rule (Expr a)
axiom4 =
    let
        g = \case
            (Eq y (Sum x Zero)) -> Eq y x
            r                   -> r
    in
        (g, g)


-- x + s(y) = s(x + y)
axiom5 :: Rule (Expr a)
axiom5 =
    let
        f = \case
            (Eq z (Sum x (Succ y))) -> Eq z (Succ (Sum x y))
            r                       -> r
        g = \case
            Eq z (Succ (Sum x y)) -> (Eq z (Sum x (Succ y))) 
            s                     -> s
    in
        (f, g)


-- x * 0 = 0
axiom6 :: Rule (Expr a)
axiom6 =
    let
        f = \case
            (Eq z (Prod _ Zero)) -> Eq z Zero
            r                    -> r
        g = \case
            -- TODO make the converse work, i.e. produce a forall statement
            r -> r
    in
        (f, g)


axiom7 :: Eq a => Rule (Expr a)
axiom7 =
    let
        f = \case
            (Eq z (Prod x (Succ y))) -> Eq z (Sum (Prod x y) x)
            r                        -> r
        g = \case
            p@(Eq z (Sum (Prod x y) r))
                | r == x -> Eq z (Prod x (Succ y))
                | otherwise -> p
            s -> s
    in
        (f, g)



-- Laws of logical deduction

doubleNegation :: Rule (Expr a)
doubleNegation =
    let
        f = \case
            (Not (Not x)) -> x
            r             -> r
    in
        (Not . Not, f)


excludedMiddle :: Eq a => Rule (Expr a)
excludedMiddle =
    let
        f = \case
            p@(Or x (Not z))
                | x == z -> Taut
                | otherwise -> p
            r -> r
    in
        (f, id)

rulesBase = []