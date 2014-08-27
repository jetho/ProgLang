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

test = mapM_ (putStrLn . format . resolveType) $ tests
    where 
        resolveType = second typeInfer
        format      = (++) <$> (++ " :: ") . fst <*> snd

tests = [
    (   "(\\x -> (fst x, 4))", 
        (EAbs "x" (EApp (EApp (EVar "Pair") (EApp (EVar "fst") (EVar "x"))) (ELit $ LInt 4 )))
    ),

    (   "(\\x -> (snd x, fst x)) (4, True)", 
        (EApp (EAbs "x" (EApp (EApp (EVar "Pair") (EApp (EVar "snd") (EVar "x"))) (EApp (EVar "fst") (EVar "x"))) ) (EApp (EApp (EVar "Pair") (ELit $ LInt 4)) (ELit $ LBool True)))
    ),

    (   "(\\x -> (fst x, fst x))",
        (EAbs "x" (EApp (EApp (EVar "Pair") (EApp (EVar "fst") (EVar "x"))) (EApp (EVar "fst") (EVar "x"))))    
    ),

    (   "let x = snd (3, True) in (x, x)",
        (ELet "x" (EApp (EVar "snd") (EApp (EApp (EVar "Pair") (ELit $ LInt 3)) (ELit $ LBool True))) (EApp (EApp (EVar "Pair") (EVar "x")) (EVar "x" )))
    ),

    (   "let swap t = (snd t, fst t) in swap",
        (ELet "swap" (EAbs "t" (EApp (EApp (EVar "Pair") (EApp (EVar "snd") (EVar "t"))) (EApp (EVar "fst") (EVar "t")))) (EVar "swap"))
    ),

    (   "(\\t -> (fst t) + (snd t))",
        (EAbs "t" (EApp (EApp (EVar "+") (EApp (EVar "fst") (EVar "t"))) (EApp (EVar "snd") (EVar "t")))) 
    ),
        
    (   "(\\f -> (f 1, 4))",
        (EAbs "f" (EApp (EApp (EVar "Pair") (EApp (EVar "f") (ELit $ LInt 1))) (ELit $ LInt 4))) 
    ),

    (   "(\\f -> inc(f 1 2))",
        (EAbs "f" (EApp (EVar "inc") (EApp (EApp (EVar "f") (ELit $ LInt 1)) (ELit $ LInt 2)))) 
    ),

    (   "(\\f -> f (f 1))",
        (EAbs "f" (EApp (EVar "f") (EApp (EVar "f") (ELit $ LInt 1)))) 
    ),
        
    (   "(\\x -> inc(snd x)) (4, True)",
        (EApp (EAbs "x" (EApp (EVar "inc") (EApp (EVar "snd") (EVar "x")))) (EApp (EApp (EVar "Pair") (ELit $ LInt 4)) (ELit $ LBool True)))
    ),

    (   "(\\f -> (f 1) + (f False))",
        (EAbs "f" (EApp (EApp (EVar "+") (EApp (EVar "f") (ELit $ LInt 1))) (EApp (EVar "f") (ELit $ LBool False)))) 
    ),
        
    (   "(\\f -> inc (f f))",
        (EAbs "f" (EApp (EVar "inc") (EApp (EVar "f") (EVar "f")))) 
    ),

    (   "(\\x -> x + y)",
        (EAbs "x" (EApp (EApp (EVar "+") (EVar "x")) (EVar "y"))) 
    ),

    (   "(\\x -> let y = x in y)",
        (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))
    ),

    (   "let g y = (let f x = (x=y) in f 1 && f false) in g 2",
        (ELet "g" (EAbs "y" (ELet "f" (EAbs "x" (EApp (EApp (EVar "=") (EVar "x")) (EVar "y"))) (EApp (EApp (EVar "&&") (EApp (EVar "f") (ELit $ LInt 1))) (EApp (EVar "f") (ELit $ LBool False))))) (EApp (EVar "g") (ELit $ LInt 2)))
    ),

    (   "(\\x -> let y = (\\z -> z) in y)",
        (EAbs "x" (ELet "y" (EAbs "z" (EVar "z")) (EVar "y")))
    ),
        
    (   "(\\x -> let y = (\\z -> x z) in y)",
        (EAbs "x" (ELet "y" (EAbs "z" (EApp (EVar "x") (EVar "z"))) (EVar "y")))
    )
    ]
