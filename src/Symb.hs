module Symb where

newtype Symb = S String deriving Eq

instance Show Symb where
    show (S str) = str

instance Enum Symb where
    -- toEnum :: Int -> Symb
    toEnum x
        | x <= 0  = S []
        | x <= 26 = S [toEnum (x + 96)]
        | otherwise = (\(S a) (S b) -> S (a ++ b)) (toEnum $ div x 26) (toEnum $ 1 + mod x 26)

    --fromEmum :: Symb -> Int
    fromEnum (S [])     = 0
    fromEnum (S (x:xs)) =
        let
            len = length xs
        in
            ((26^len) * (fromEnum x - 96)) + fromEnum (S xs)


newtype ST a = St (Symb -> (a, Symb))

nextState :: ST a -> Symb -> (a, Symb)
nextState (St st) = st

instance Monad ST  where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    (St f) >>= g = St $ \symb ->
        let
            (a, symb')  = f symb
        in
            nextState (g a) symb'

instance Functor ST where
    -- fmap :: (a -> b) -> (ST a -> ST b)
    fmap g st = do
        g <$> st

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = St $ \symb -> (x, symb)

    -- (<*>) :: ST (a -> b) -> (ST a -> ST b)
    stf <*> sta = do
        f <- stf
        f <$> sta


fresh :: ST Symb
fresh = St $ \symb -> (symb, succ symb)


freshList :: [a] -> [Symb]
freshList ls = fst $ nextState (h_fresh ls) (S "a")
    where
        h_fresh :: [a] -> ST [Symb]
        h_fresh [] = do
            return []
        h_fresh (_:xs) = do
            s <- fresh
            rest <- h_fresh xs
            return $ s:rest
