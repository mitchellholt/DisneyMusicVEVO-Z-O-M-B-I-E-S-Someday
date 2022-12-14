{-# LANGUAGE LambdaCase #-}
module Rule where

import Statement

type Rule = (
    Stmt (Expr String) -> Stmt (Expr String),
    Stmt (Expr String) -> Stmt (Expr String),
    String
    )

-- instance Eq Rule where
--     (_,_,s) == (_,_,t) = s == t

not :: Rule
not = (Not, Not, "not")


associativity :: Rule
associativity =
    let 
        f = \case
            (Or p (Or q r)) -> (Or (Or p q) r)
            s               -> s
        g = \case
            (Or (Or p q) r) -> (Or p (Or q r))
            s               -> s
    in
        (f, g, "associativity")


-- stbs f = (Eq x y -> Eq (f x) (f y), Eq (f x) (f y) -> Eq (f x) (f y)
stbs :: (Expr String -> Expr String) -> Rule
stbs f =
    let
        g = \case
            (Eq x y) -> Eq (f x) (f y)
            r        -> r
            
    in
        (g, id, "stbs")


commute :: Rule
commute =
    let
        g = \case
            (Eq x y) -> Eq y x
            (Or x y) -> Or y x
            r        -> r
    in
        (g, g, "commute")


-- A x, ~(0 = s(x))
axiom1 :: Rule
axiom1 =
    let
        g = \case
            -- s@(Forall (Var x) (Not (Eq Zero (Succ y))))
            --     | y == Var x -> Taut
            --     | otherwise -> s

            (Forall (Var _) (Not (Eq Zero (Succ _)))) -> Taut
            r -> r
    in
        (id, g, "axiom 1")


-- forall x, forall y, sx = sy => x = y
axiom2 :: Rule
axiom2 =
    let
        f = \case
            (Eq (Succ x) (Succ y)) -> Eq x y
            r                      -> r
        g = \case
            (Eq x y) -> Succ <$> Eq x y
            s        -> s
    in
        (f, g, "axiom 2")


-- forall y, (y = 0) or (exists x : s(x) = y)
axiom3 :: Rule
axiom3 = 
    let
        g = \case
            Or (Eq y Zero) (Exists (Var x) (Eq (Succ (Var xx)) z)) -> if ((y == z) && (x == xx) ) then Taut else Or (Eq y Zero) (Exists (Var x) (Eq (Succ (Var xx)) z))
            -- p@(Exists x (Eq (Succ z) y))
            --     | x == z    -> (Eq y Zero)
            --     | otherwise -> p
            -- -- TODO put converse here
            r -> r
    in 
        (g, g, "axiom 3")


-- Forall x, x + 0 = x
axiom4 :: Rule
axiom4 =
    let
        g = \case
            (Eq (Sum x Zero) y) -> Eq x y
            r                   -> r
    in
        (g, g, "axiom 4")


-- x + s(y) = s(x + y)
axiom5 :: Rule
axiom5 =
    let
        f = \case
            (Eq z (Sum x (Succ y))) -> Eq z (Succ (Sum x y))
            r                       -> r
        g = \case
            Eq (Sum x (Succ y)) (Succ (Sum xx yy)) -> if ((y == yy) && (x == xx)) then Taut else Eq (Sum x (Succ y)) (Succ (Sum xx yy))
            s                     -> s
    in
        (f, g, "axiom 5")


-- x * 0 = 0
axiom6 :: Rule
axiom6 =
    let
        f = \case
            (Eq z (Prod _ Zero)) -> Eq z Zero
            r                    -> r
        g = \case
            (Eq z (Prod _ Zero)) -> Taut
            (Eq (Prod _ Zero) z) -> Taut
            r -> r
    in
        (f, g, "axoim 6")


axiom7 :: Rule
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
        (f, g, "axiom 7")



-- Laws of logical deduction

doubleNegation :: Rule
doubleNegation =
    let
        f = \case
            (Not (Not x)) -> x
            r             -> r
    in
        (Not . Not, f, "double negation")


excludedMiddle :: Rule
excludedMiddle =
    let
        f = \case
            p@(Or x (Not z))
                | x == z -> Taut
                | otherwise -> p
            r -> r
    in
        (f, id, "excluded middle")


defaultRules :: [Rule]
defaultRules = [
    associativity,
    commute,
    axiom1,
    axiom2,
    axiom3,
    axiom4,
    axiom5,
    axiom6,
    axiom7,
    doubleNegation,
    excludedMiddle,
    (id,id,"id"),
    (id,(\x ->
        case x of
            Eq (Succ a) (Succ b) -> Eq a b
            y -> y
    ),"bothsucc"),
    (id,(\x ->
        case x of
            Eq a b -> Eq (Succ a) (Succ b)
            y -> y
    ),"succboth"),
    (id,(\x ->
        case x of
            Eq a b -> if (a == b) then Taut else Eq a b
            y -> y
    ),"eq"),
    (id,(\x ->
        case x of
            Forall _ Taut -> Taut
            Exists _ Taut -> Taut
            y -> y
    ),"alltrue")
               
    
    ]
