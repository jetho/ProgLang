module Context where

newtype Context = Context [String]

emptyContext = Context []

addBinding :: Context -> String -> Context
addBinding (Context bs) b = Context $ b:bs

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx@(Context bindings) name =
    (ctx', freshName)
    where
        ctx' = addBinding ctx freshName
        freshName = getName name bindings
        getName name [] = name
        getName name (n:ns) | n == name = getName (name ++ "'") bindings
                            | otherwise = getName name ns

index2name :: Context -> Int -> String
index2name (Context ns) index =
    if length ns > index
    then ns !! index
    else error $ "Invalid index " ++ show index ++ " in context with length " ++ show (length ns)

ctxLength :: Context -> Int
ctxLength (Context bindings) = length bindings

