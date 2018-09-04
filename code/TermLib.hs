module TermLib where

data Term
    = Cond Term Term Term       -- Cond test then-branch else-branch
    | Lambda String Term        -- Lambda var body
    | Let [(String, Term)] Term -- Let [(name, rhs), ...] body
    | Prim2 String Term Term    -- Prim2 opname operand operand
    | App Term Term             -- App func arg
    | Num Integer
    | Bln Bool
    | Var String
    deriving (Eq, Show)
