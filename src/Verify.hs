expand :: (Stmt a) -> (Stmt a)
expand Taut = Taut
expand (Or a _) = a
expand (Forall a s) = s

bot :: (Pf a) -> (Stmt a)
bot (Truth (s,_)) = s
bot (Expand _ _ _ p) = bot p
bot (Case _ _ _ _ p) = bot p
bot (Apply _ p) = bot p

top :: (Pf a) -> (Stmt a)
top (Expand (s,_) _ _) = s
top (Case (s,_) _ _ _ _) = s
top (Apply (s,_) _ _) = s
top (Truth (s,_)) = s


split :: (Stmt a) -> (Stmt a -> Stmt a, Stmt a)
split (Or a b) = (\x -> (Or x b), a)
split (Not s) = (Not, s)
split (Exists a s) = (Exists a, s)
split (Forall a s) = (Forall a, s)
split x = (\y -> x, x)

verifyS :: (Pf a) -> Bool
verifyS (Truth _) -> True
verifyS (Apply (s,_) r p) = ((top p) == (r s)) & (verifyS p)
verifyS (Case (s,_) r t f n) = and $ (fmap verifyS [t, f, n]) ++ (fmap (\x -> s == (top x)) [t, f, n]) ++ [(bot t) == (top n), (bot f) == (top n)]
verifyS (Expand (s,_) p n) = let (f, ss) = (split s) in 
    ((top p) == ss) & ((f $ bot p) == (top n)) & (verifyS n) & (verifyS p)