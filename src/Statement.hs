module Statement where

data Expr a = Zero | Var a | Succ (Expr a) | Prod (Expr a) (Expr a)
    | Sum (Expr a) (Expr a) deriving (Eq, Show)


instance Monad Expr where
    -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    Zero >>= _ = Zero
    (Var a) >>= f = f a
    (Succ expr) >>= f = do
        a <- expr
        f a
    (Prod el er) >>= f = do
        l <- el
        r <- er
        Prod (f l) (f r)
    (Sum el er) >>= f = do
        l <- el
        r <- er
        Sum (f l) (f r)

instance Applicative Expr where
    -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    pure = Var
    func <*> expr = do
        f <- func
        f <$> expr

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap f expr = do
        f <$> expr




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
