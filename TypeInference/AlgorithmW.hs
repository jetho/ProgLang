module AlgorithmW where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import qualified Text.PrettyPrint as PP


data Type = TVar String
          | TInt
          | TBool
          | TFun Type Type
          deriving (Eq, Ord)

data Scheme = Scheme [String] Type

data Exp = EVar String
         | ELit Lit
         | EApp Exp Exp
         | EAbs String Exp
         | ELet String Exp Exp
         deriving (Eq, Ord)

data Lit = LInt Integer
         | LBool Bool
         deriving (Eq, Ord)

