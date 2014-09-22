module Term where

import Context

data Term 
    = Var Int Int
    | Abs String Term
    | App Term Term
    deriving (Eq, Show)

isVal :: Term -> Bool
isVal (Abs _ _) = True
isVal _           = False
     
printTerm (Abs x t1) ctx = "(λ" ++ x' ++ "." ++ (printTerm t1 ctx') ++ ")"
    where (ctx', x') = pickFreshName ctx x
printTerm (Var i n) ctx = 
    if n == ctxLength ctx 
    then index2name ctx i
    else error "[bad index]"
printTerm (App t1 t2) ctx = printTerm t1 ctx ++ " " ++ printTerm t2 ctx

