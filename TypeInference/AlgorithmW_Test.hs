module TypeInference.AlgorithmW_Test (test) where

import TypeInference.AlgorithmW
import Data.Map as Map
import Control.Arrow (second)
import Control.Applicative


env = TypeEnv $ Map.fromList [ 
    ("+", Scheme [] (TFun TInt (TFun TInt TInt))),
    ("*", Scheme [] (TFun TInt (TFun TInt TInt))),
    ("inc", Scheme [] (TFun TInt TInt)),
    ("Pair", Scheme ["a", "b"] (TFun (TVar "a") (TFun (TVar "b") (TPair (TVar "a") (TVar "b")))) ), 
    ("fst", Scheme ["a", "b"] (TFun (TPair (TVar "a") (TVar "b")) (TVar "a"))),
    ("snd", Scheme ["a", "b"] (TFun (TPair (TVar "a") (TVar "b")) (TVar "b"))),
    ("swap", Scheme ["a", "b"] (TFun (TPair (TVar "a") (TVar "b")) (TPair (TVar "b") (TVar "a")))),
    ("=", Scheme ["a"] (TFun (TVar "a") (TFun (TVar "a") TBool))),
    ("&&", Scheme [] (TFun TBool (TFun TBool TBool)))
    ]


typeInfer e = case typeInference env e of
                  Left err -> "Type Error: " ++ show err
                  Right t  -> show t

test = mapM_ (putStrLn . format . resolveType) $ exps
    where 
        resolveType = second typeInfer
        format      = (++) <$> (++ " :: ") . fst <*> snd
        exps        = [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14]


e0 = (expr, ast)
    where 
        expr = "(\\x -> (fst x, 4))"
        ast  = (EAbs "x" (EApp (EApp (EVar "Pair") (EApp (EVar "fst") (EVar "x"))) (ELit $ LInt 4 ))) 

e1 = (expr, ast) 
    where
        expr = "(\\x -> (snd x, fst x)) (4, True)"
        ast  = (EApp (EAbs "x" (EApp (EApp (EVar "Pair") (EApp (EVar "snd") (EVar "x"))) (EApp (EVar "fst") (EVar "x"))) ) (EApp (EApp (EVar "Pair") (ELit $ LInt 4)) (ELit $ LBool True)))

e2 = (expr, ast)
    where
        expr = "(\\x -> (fst x, fst x))"
        ast  = (EAbs "x" (EApp (EApp (EVar "Pair") (EApp (EVar "fst") (EVar "x"))) (EApp (EVar "fst") (EVar "x"))) ) 

e3 = (expr, ast)
    where
        expr = "let x = snd (3, True) in (x, x)"
        ast  = (ELet "x" (EApp (EVar "snd") (EApp (EApp (EVar "Pair") (ELit $ LInt 3)) (ELit $ LBool True))) (EApp (EApp (EVar "Pair") (EVar "x")) (EVar "x" )))  

e4 = (expr, ast)
    where
        expr = "let swap t = (snd t, fst t) in swap"
        ast  = (ELet "swap" (EAbs "t" (EApp (EApp (EVar "Pair") (EApp (EVar "snd") (EVar "t"))) (EApp (EVar "fst") (EVar "t")))) (EVar "swap"))

e5 = (expr, ast)
    where
        expr = "(\\t -> (fst t) + (snd t))"
        ast  = (EAbs "t" (EApp (EApp (EVar "+") (EApp (EVar "fst") (EVar "t"))) (EApp (EVar "snd") (EVar "t")))) 
        
e6 = (expr, ast)
    where 
        expr = "(\\f -> (f 1, 4))"
        ast  = (EAbs "f" (EApp (EApp (EVar "Pair") (EApp (EVar "f") (ELit $ LInt 1))) (ELit $ LInt 4))) 

e7 = (expr, ast)
    where 
        expr = "(\\f -> inc(f 1 2))"
        ast  = (EAbs "f" (EApp (EVar "inc") (EApp (EApp (EVar "f") (ELit $ LInt 1)) (ELit $ LInt 2)))) 

e8 = (expr, ast)
    where 
        expr = "(\\f -> f (f 1))"
        ast  = (EAbs "f" (EApp (EVar "f") (EApp (EVar "f") (ELit $ LInt 1)))) 
        
e9 = (expr, ast)
    where 
        expr = "(\\x -> inc(snd x)) (4, True)"
        ast  = (EApp (EAbs "x" (EApp (EVar "inc") (EApp (EVar "snd") (EVar "x")))) (EApp (EApp (EVar "Pair") (ELit $ LInt 4)) (ELit $ LBool True)))

e10 = (expr, ast)
    where
        expr = "(\\f -> (f 1) + (f False))"
        ast  = (EAbs "f" (EApp (EApp (EVar "+") (EApp (EVar "f") (ELit $ LInt 1))) (EApp (EVar "f") (ELit $ LBool False)))) 
        
e11 = (expr, ast)
    where 
        expr = "(\\f -> inc (f f))"
        ast  = (EAbs "f" (EApp (EVar "inc") (EApp (EVar "f") (EVar "f")))) 

e12 = (expr, ast)
    where 
        expr = "(\\x -> x + y)"
        ast  = (EAbs "x" (EApp (EApp (EVar "+") (EVar "x")) (EVar "y"))) 

e13 = (expr, ast)
    where
        expr = "(\\x -> let y = x in y)"
        ast  = (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))

e14 = (expr, ast)
    where
        expr = "let g y = (let f x = (x=y) in f 1 && f false) in g 2"
        ast  = (ELet "g" (EAbs "y" (ELet "f" (EAbs "x" (EApp (EApp (EVar "=") (EVar "x")) (EVar "y"))) (EApp (EApp (EVar "&&") (EApp (EVar "f") (ELit $ LInt 1))) (EApp (EVar "f") (ELit $ LBool False))))) (EApp (EVar "g") (ELit $ LInt 2)))
