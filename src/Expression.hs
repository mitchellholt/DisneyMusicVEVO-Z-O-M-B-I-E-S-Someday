module Expression where

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
