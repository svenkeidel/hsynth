module Language.Compile where

import           Control.Monad.Except

import           LLVM.General.AST (Module)
import qualified LLVM.General.Module as L
import qualified LLVM.General.Context as L

import           Language.CodeGen.LLVM
import           Language.Expression
import qualified Language.Expression as E
import           Language.Product
import           Language.SimpleExpression
import qualified Language.SimpleExpression as S
import           Language.SynArrow

compile :: SynArrow SimpleExpr Function b Double -> Module
compile f = case optimize f of
  LoopD i (Arr (Function g)) -> modul "test" $ assignments (inj (S.Const 0) i) $ g E.Var

compile' :: SynArrow SimpleExpr Function b Double -> IO (Either String String)
compile' s = L.withContext $ \ctx ->
             runExceptT $ L.withModuleFromAST ctx (compile s) $ \m ->
                 L.moduleLLVMAssembly m
