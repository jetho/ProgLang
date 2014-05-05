module TypeInference.AlgorithmW_Test (test) where

import TypeInference.AlgorithmW
import Data.Map as Map
import Control.Arrow (second)


env = TypeEnv $ Map.fromList [ 
    ("+", Scheme [] (TFun TInt (TFun TInt TInt))),
    ("*", Scheme [] (TFun TInt (TFun TInt TInt))),
    ("Pair", Scheme ["a", "b"] (TFun (TVar "a") (TFun (TVar "b") (TPair (TVar "a") (TVar "b")))) ), 
    ("fst", Scheme ["a", "b"] (TFun (TPair (TVar "a") (TVar "b")) (TVar "a"))),
    ("snd", Scheme ["a", "b"] (TFun (TPair (TVar "a") (TVar "b")) (TVar "b"))),
    ("swap", Scheme ["a", "b"] (TFun (TPair (TVar "a") (TVar "b")) (TPair (TVar "b") (TVar "a"))))
    ]


typeInfer e = case typeInference env e of
                  Left err -> "Type Error: " ++ show err
                  Right t  -> show t

test = mapM_ (putStrLn . format . resolveType) $ exps
    where 
          resolveType     = second typeInfer
          format (s1, s2) = s1 ++ " :: " ++ s2
          exps            = [e0, e1, e2, e3]


e0 = (expr, ast)
    where 
          expr = "(\\x -> (fst x, 4))"
          ast  = (EAbs "x" (EApp (EApp (EVar "Pair") (EApp (EVar "fst") (EVar "x"))) (ELit $ LInt 4 ))) 

e1 = (expr, ast) 
    where
        expr = "((\\x -> (fst x, snd x)) (4, True)"
        ast  = (EApp (EAbs "x" (EApp (EApp (EVar "Pair") (EApp (EVar "fst") (EVar "x"))) (EApp (EVar "snd") (EVar "x"))) ) (EApp (EApp (EVar "Pair") (ELit $ LInt 4)) (ELit $ LBool True)))

e2 = (expr, ast)
    where
        expr = "(\\x -> (fst x, fst x))"
        ast  = (EAbs "x" (EApp (EApp (EVar "Pair") (EApp (EVar "fst") (EVar "x"))) (EApp (EVar "fst") (EVar "x"))) ) 

e3 = (expr, ast)
    where
        expr = "let x = snd (3, True) in (x, x)"
        ast  = (ELet "x" (EApp (EVar "snd") (EApp (EApp (EVar "Pair") (ELit $ LInt 3)) (ELit $ LBool True))) (EApp (EApp (EVar "Pair") (EVar "x")) (EVar "x" )))  

