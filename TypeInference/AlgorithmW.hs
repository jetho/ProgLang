{-# LANGUAGE PackageImports #-}


module AlgorithmW where

import qualified Data.Map as M
import qualified Data.Set as S
import "mtl" Control.Monad.Error
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Identity
import qualified Text.PrettyPrint as PP

data Exp = EVar String
         | ELit Lit
         | EApp Exp Exp
         | EAbs String Exp
         | ELet String Exp Exp
         deriving (Eq, Ord)

data Lit = LInt Integer
         | LBool Bool
         deriving (Eq, Ord)

data Type = TVar String
          | TInt
          | TBool
          | TFun Type Type
          deriving (Eq, Ord)

data Scheme = Scheme [String] Type

type Subst = M.Map String Type

newtype TypeEnv = TypeEnv (M.Map String Scheme)

data TIEnv = TIEnv {}

data TIState = TIState { tiSupply :: Int,
                         tiSubst :: Subst}

type TI a = ErrorT TypeError (ReaderT TIEnv (StateT TIState Identity)) a

data TypeError = UnboundVariable String
               | NotUnifiable Type Type
               | Circularity String Type
               | OtherError String

instance Error TypeError where
    noMsg = OtherError "A Type Error!"
    strMsg s = OtherError s

class Types a where
    ftv   :: a -> S.Set String
    subst :: Subst -> a -> a

instance Types Type where
    ftv (TVar n)         = S.singleton n
    ftv TInt             = S.empty
    ftv TBool            = S.empty
    ftv (TFun t1 t2)     = ftv t1 `S.union` ftv t2
    subst s v@(TVar n)   = maybe v id $ M.lookup n s
    subst s (TFun t1 t2) = TFun (subst s t1) (subst s t2)
    subst _ t            = t

instance Types Scheme where
    ftv (Scheme vars t)     = (ftv t) `S.difference` (S.fromList vars)
    subst s (Scheme vars t) = Scheme vars (subst (foldr M.delete s vars) t)

instance Types a => Types [a] where
    ftv   = foldr S.union S.empty . map ftv
    subst = map . subst

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (M.elems env)
    subst s (TypeEnv env) = TypeEnv (M.map (subst s) env)

nullSubst :: Subst
nullSubst = M.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.map (subst s1) s2 `M.union` s1

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (M.delete var env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = S.toList ((ftv t) `S.difference` (ftv env))
