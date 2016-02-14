{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Expression.Untyped where

import qualified Language.Expression.Typed as Typed

data Expr where
  Var :: Expr
  Const :: Double -> Expr

  App :: Expr -> Expr -> Expr
  Fun :: Typed.Fun a -> Expr

  Inj :: Expr -> Expr -> Expr
  Proj1 :: Expr -> Expr
  Proj2 :: Expr -> Expr

deriving instance Show Expr

lowerExpr :: Typed.Expr a -> Expr
lowerExpr e =
  case e of
    Typed.Var -> Var
    Typed.Const c -> Const c
    Typed.App e1 e2 -> App (lowerExpr e1) (lowerExpr e2)
    Typed.Fun f -> Fun f

    Typed.Inj e1 e2 -> Inj (lowerExpr e1) (lowerExpr e2)
    Typed.Proj1 e1 -> Proj1 (lowerExpr e1)
    Typed.Proj2 e1 -> Proj2 (lowerExpr e1)

data Prod where
  Prod :: Prod -> Prod -> Prod
  Unit :: Prod
  Constant :: Double -> Prod
  deriving Show

lowerProduct :: Typed.Prod a -> Prod
lowerProduct p = case p of
  Typed.Prod p1 p2 -> Prod (lowerProduct p1) (lowerProduct p2)
  Typed.Unit -> Unit
  Typed.Constant d -> Constant d

lineralize :: Expr -> Expr
lineralize e = case e of
  Inj (Inj e1 e2) e3 -> lineralize (Inj e1 (Inj e2 e3))
  Inj e1 e2 -> Inj e1 (lineralize e2)
  e1 -> e1
