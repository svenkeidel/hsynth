{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.CodeGen where

import           Control.Monad.Writer
import           Control.Monad.State

import           Data.Foldable(toList)
import qualified Language.Expression.Typed as Typed
import qualified Language.Expression.Untyped as Untyped
import           Data.Sequence (Seq,(<|),(|>),(><))
import qualified Data.Sequence as S

import           LLVM.General.AST (Module,Definition,BasicBlock,Named,Instruction)
import qualified LLVM.General.AST as L
import qualified LLVM.General.AST.Linkage as Linkage
import qualified LLVM.General.AST.Visibility as Visibility
import qualified LLVM.General.AST.CallingConvention as CallingConvention
import qualified LLVM.General.AST.Type as Type
import qualified LLVM.General.AST.Constant as Constant
import qualified LLVM.General.AST.Float as Float

data Two = One | Two

instance Show Two where
  show One = "1"
  show Two = "2"

data Expr a where
  Var :: Seq Two -> Expr Double
  Const :: Double -> Expr Double

  App :: Expr (a -> b) -> Expr a -> Expr b
  Fun :: Typed.Fun (a -> b) -> Expr (a -> b)

deriving instance Show (Expr a)

data Assignment where
  (:=) :: Seq Two -> Expr Double -> Assignment

deriving instance Show Assignment

gen :: String -> Target -> Module
gen n ass = L.defaultModule
  { L.moduleName = n
  , L.moduleDefinitions = return $ mainFunction ass
  }

data Target = Target Untyped.Prod (Seq Assignment)

mainFunction :: Target -> Definition
mainFunction ass =
  L.GlobalDefinition $ L.Function Linkage.Internal
                       Visibility.Default
                       Nothing
                       CallingConvention.Fast
                       []
                       (Type.IntegerType 32)
                       (L.Name "main")
                       ([],False)
                       []
                       Nothing
                       Nothing
                       0
                       Nothing
                       Nothing
                       $ mainBody ass

mainBody :: Target -> [BasicBlock]
mainBody (Target initial ass) =
  [ initalBlock initial
  , loopBlock ass
  ]

initalBlock :: Untyped.Prod -> BasicBlock
initalBlock prod =
  L.BasicBlock
    (L.Name "entry")
    (initInstructions prod)
    (L.Do (L.Br (L.Name "loop") []))

  where
    --initInstructions :: Untyped.Prod -> [Named Instruction]
    initInstructions = undefined

genAssignment :: (MonadWriter (Seq (Named Instruction)) m, MonadState Word m) => Assignment -> m ()
genAssignment (to := from) = do
  lastName <- genExpr from
  tell $ S.singleton $ name to L.:= L.Add False False (L.ConstantOperand (Constant.Float (Float.Double 0))) (L.LocalReference Type.double lastName) []
  where
    genExpr :: (MonadWriter (Seq (Named Instruction)) m, MonadState Word m) => Expr Double -> m L.Name
    genExpr expr = case expr of
      Var n -> return $ name n
      Const c -> do
        x <- fresh
        tell $ S.singleton $ x L.:= L.Add False False (L.ConstantOperand (Constant.Float (Float.Double c))) (L.ConstantOperand (Constant.Float (Float.Double c))) []
        return x
      App (App (Fun Typed.Add) e1) e2 -> do
        x1 <- genExpr e1
        x2 <- genExpr e2
        x3 <- fresh
        tell $ S.singleton $ x3 L.:= L.Add False False (L.LocalReference Type.double x1) (L.LocalReference Type.double x2) []
        return x3

    fresh :: MonadState Word m => m L.Name
    fresh = state (\i -> (L.UnName i, i+1))

name :: Seq Two -> L.Name
name ids = L.Name $ concat $ toList $ fmap show ids

loopBlock :: Seq Assignment -> BasicBlock
loopBlock = undefined

assignments :: Typed.Expr e -> Seq Assignment
assignments = go S.empty . Typed.optimizeExpr
  where
    go :: Seq Two -> Typed.Expr e -> Seq Assignment
    go to e = case e of
      Typed.Inj e1 e2 ->
        go (to |> One) e1 >< go (to |> Two) e2
      e1 -> case lowerExpr S.empty e1 of
        Just expr -> return $ to := expr
        Nothing -> error "Expr is not ground."

pattern TFun2 f e1 e2 = Typed.App (Typed.App (Typed.Fun f) e1) e2
pattern Fun2 f e1 e2 = App (App (Fun f) e1) e2

lowerExpr :: Seq Two -> Typed.Expr a -> Maybe (Expr Double)
lowerExpr addr expr = case expr of
  Typed.Proj1 e1 -> lowerExpr (One <| addr) e1
  Typed.Proj2 e1 -> lowerExpr (Two <| addr) e1
  Typed.Var -> Just $ Var addr
  Typed.Const d -> Just $ Const d
  TFun2 Typed.Add e1 e2 -> Fun2 Typed.Add <$> lowerExpr addr e1 <*> lowerExpr addr e2
  TFun2 Typed.Mult e1 e2 -> Fun2 Typed.Mult <$> lowerExpr addr e1 <*> lowerExpr addr e2
  TFun2 Typed.Sub e1 e2 -> Fun2 Typed.Sub <$> lowerExpr addr e1 <*> lowerExpr addr e2
  TFun2 Typed.Sub e1 e2 -> Fun2 Typed.Sub <$> lowerExpr addr e1 <*> lowerExpr addr e2
  _ -> Nothing
