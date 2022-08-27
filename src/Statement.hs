module Statement where

import Expression

-- Type used on statement should be epression
-- REQUIRE Exists a is (Var x)
data Stmt a =  Taut | Eq a a | Or (Stmt a) (Stmt a) | Not (Stmt a)
            | Exists a (Stmt a) | Forall a (Stmt a) deriving (Eq, Show)


instance Functor Stmt where
    -- fmap :: (a -> b) -> Stmt a -> Stmt b
    fmap _ Taut = Taut
    fmap f (Eq x y) = Eq (f x) (f y)
    fmap f (Or x y) = Or (f <$> x) (f <$> y)
    fmap f (Not x) = Not (f <$> x)
    fmap f (Exists a stmt) = Exists (f a) (f <$> stmt)
    fmap f (Forall a stmt) = Forall (f a) (f <$> stmt)

implies :: Stmt a -> Stmt a -> Stmt a
implies p = Or (Not p)


and :: Stmt a -> Stmt a -> Stmt a
and p q = Not (Or (Not p) (Not q))


negate :: Stmt a -> Stmt a
negate Taut = Not Taut
negate (Eq x y) = Not (Eq x y)
negate (Or x y) = Not (Or x y)
negate (Not x) = x
negate (Exists x p) = Forall x (Not p)
negate (Forall x p) = Exists x (Not p)
