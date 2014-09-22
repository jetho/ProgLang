module Evaluator where

import Context
import Term

mapTerm term handler = walk 0 term
    where walk c v@(Var _ _) = handler c v
          walk c (Abs x t1)  = Abs x $ walk (c+1) t1
          walk c (App t1 t2) = App (walk c t1) (walk c t2)

shift inc term = mapTerm term shiftVar
    where shiftVar c (Var i n) | i >= c     = Var (i+inc) (n+inc)
                               | otherwise  = Var i (n+inc)

subst substIdx replacement term = mapTerm term substVar
    where substVar c v@(Var i n) | i == c+substIdx  = shift c replacement
                                 | otherwise        = v

termSubstTop s t = shift (-1) (subst 0 (shift 1 s) t)

eval1 ctx (App (Abs _ t1) t2) | isVal t2 = termSubstTop t2 t1
eval1 ctx (App v1 t2)         | isVal v1 = App v1 (eval1 ctx t2)
eval1 ctx (App t1 t2)                    = App (eval1 ctx t1) t2
eval1 ctx t                              = t

eval ctx t = if t==t' then t else eval1 ctx t'
    where t' = eval1 ctx t


