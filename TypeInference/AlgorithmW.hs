
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}

module AlgorithmW where

import qualified Data.Map as Map
import qualified Data.Set as Set
import "mtl" Control.Monad.Error
import "mtl" Control.Monad.State
import qualified Text.PrettyPrint as PP


type EVar = String
type TVar = String


data Exp = EVar EVar
         | ELit Lit
         | EApp Exp Exp
         | EAbs String Exp
         | ELet String Exp Exp
         deriving (Eq, Ord)

data Lit = LInt Integer
         | LBool Bool
         deriving (Eq, Ord)

data Type = TVar TVar
          | TInt
          | TBool
          | TFun Type Type
          deriving (Eq, Ord)

data Scheme = Scheme [TVar] Type

newtype TypeEnv = TypeEnv (Map.Map EVar Scheme)

type Subst = Map.Map TVar Type


class Substitutable a where 
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set TVar

instance Substitutable Type where
    apply _  TInt         = TInt
    apply _  TBool        = TBool
    apply s  t@(TVar a)   = Map.findWithDefault t a s
    apply s  (TFun t1 t2) = TFun (apply s t1) (apply s t2)
    ftv TInt              = Set.empty
    ftv TBool             = Set.empty
    ftv (TVar a)          = Set.singleton a
    ftv (TFun t1 t2)      = (ftv t1) `Set.union` (ftv t2)


instance Substitutable Scheme where
    apply s (Scheme vars t) = Scheme vars $ apply s' t 
                              where s' = foldr Map.delete s vars
    ftv (Scheme vars t)     = (ftv t) `Set.difference` (Set.fromList vars)


instance Substitutable a => Substitutable [a] where
    apply = map . apply
    ftv   = foldr Set.union Set.empty . map ftv


instance Substitutable TypeEnv where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env)     = ftv $ Map.elems env


data TIState = TIState { tiSupply :: Int }

type TI a = ErrorT TypeError (State TIState) a

data TypeError = UnboundVariable String
               | NotUnifiable Type Type
               | Circularity String Type
               | OtherError String

instance Error TypeError where
    noMsg    = OtherError "A Type Error!"
    strMsg s = OtherError s


update :: TypeEnv -> EVar -> Scheme -> TypeEnv
update (TypeEnv env) x s =  TypeEnv $ Map.insert x s env

nullSubst :: Subst
nullSubst = Map.empty

(◦) :: Subst -> Subst -> Subst
s1 ◦ s2 = Map.map (apply s1) s2 `Map.union` s1

generalize :: TypeEnv -> Type -> Scheme
generalize env t  =   Scheme vars t
    where vars = Set.toList $ (ftv t) `Set.difference` (ftv env)

fresh :: TI Int
fresh = do s     <- get
           let n  = tiSupply s
           put $ s { tiSupply = n + 1 }
           return n

freshTVar :: String -> TI Type
freshTVar prefix = fresh >>= return . TVar . (prefix ++) . show 

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = 
    do vars' <- mapM (const $ freshTVar "a") vars
       let s  = Map.fromList $ zip vars vars'
       return $ apply s t

ti :: TypeEnv -> Exp -> TI (Subst, Type)
ti _ (ELit (LInt _))  = noSubst TInt
ti _ (ELit (LBool _)) = noSubst TBool

ti (TypeEnv env) (EVar x) = 
    maybe (throwError $ UnboundVariable x)
          (instantiate >=> noSubst)
          (Map.lookup x env) 

ti env (EAbs x e) =
    do tv       <- freshTVar "a"
       let env'  = update env x $ Scheme [] tv
       (s1, t1) <- ti env' e
       return (s1, TFun (apply s1 tv) t1)

ti env (EApp e1 e2) =
    do  tv       <- freshTVar "a"
        (s1, t1) <- ti env e1
        (s2, t2) <- ti (apply s1 env) e2
        s3       <- unify (apply s2 t1) (TFun t2 tv)
        return (s3 ◦ s2 ◦ s1, apply s3 tv)

ti env (ELet x e1 e2) =
    do (s1, t1) <- ti env e1
       let env'  = apply s1 env
           t'    = generalize env' t1
           env'' = update env' x t'
       (s2, t2) <- ti env'' e2
       return (s1 ◦ s2, t2)

noSubst :: Type -> TI (Subst, Type)
noSubst = return . (nullSubst,)

unify = undefined
