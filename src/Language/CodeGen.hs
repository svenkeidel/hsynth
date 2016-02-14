{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.CodeGen where

import qualified Language.Expression.Typed as Typed
import           Data.Sequence (Seq,(<|),(|>),(><))
import qualified Data.Sequence as S

data Two = One | Two

instance Show Two where
  show One = "1"
  show Two = "2"

data Expr where
  Var :: Seq Two -> Expr
  Const :: Double -> Expr

  App :: Expr -> Expr -> Expr
  Fun :: Typed.Fun a -> Expr

deriving instance Show Expr

data Assignment = Seq Two := Expr
  deriving Show

assignments :: Typed.Expr e -> Seq Assignment
assignments = go S.empty . Typed.optimizeExpr
  where
    go :: Seq Two -> Typed.Expr e -> Seq Assignment
    go to e = case e of
      Typed.Inj e1 e2 ->
        go (to |> One) e1 >< go (to |> Two) e2
      e1 -> return $ to := lowerExpr S.empty e1

lowerExpr :: Seq Two -> Typed.Expr e -> Expr
lowerExpr from e = case e of
  Typed.Proj1 e1 -> lowerExpr (One <| from) e1
  Typed.Proj2 e1 -> lowerExpr (Two <| from) e1
  Typed.Var -> Var from
  Typed.Const i -> Const i
  Typed.App e1 e2 -> App (lowerExpr from e1) (lowerExpr from e2)
  Typed.Fun f -> Fun f
  Typed.Inj _ _ -> error "intermediate injections should have been optimized away"
