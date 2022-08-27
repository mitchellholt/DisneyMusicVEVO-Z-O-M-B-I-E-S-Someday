# EPIC HACKATHON PROJECT

## Types

- Rule :: a -> a

- Env = (Stmt, [Rule])

- data Pf a = Expand (Env a) (Pf a) | Case (Env a) (Rule a) (Pf a) (Pf a)
| Truth (Env a)

- Exapand Rules:
    - Pf A x s : Fix x, Pf s
    - Pf E x s : Proudce x, Pf s -- define x
    - Pf Not s : Pf (s -> Not Taut)
    - Pf Or a b : Pf a -- can use commute
    - Pf Eq a b : Pf Taut -> Eq a b
    - Pf P(x) s : Pf s

- want `fold . Taut [Rule] Stmt1 == Stmt2`

## Modules

- Proof.hs : Env, Pf, --Mitchell
    - Expression.hs
    - Statment.hs

- Statement.hs : Stmt, Expr --Joel

- Rule.hs --Joel
    - Rule :: a -> a

- Parsing --Mitchell

- Verify.hs --Joel
