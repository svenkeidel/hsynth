{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
module Language.GroundExpr(GroundExpr(..),Floor(..),floor,Two(..)) where

import           Prelude hiding (floor)

import           Data.Text (Text)

import           Data.Sequence (Seq,(<|),(|>))
import qualified Data.Sequence as S

import Language.Expression (Expr)
import qualified Language.Expression as Full
import Language.Haskell.TH.Syntax

data Two = One | Two
  deriving (Eq,Ord)

instance Show Two where
  show One = "1"
  show Two = "2"

data GroundExpr a where
  Var :: Seq Two -> GroundExpr a
  Const :: (Lift a, Show a) => a -> GroundExpr a

  If :: GroundExpr Bool -> GroundExpr a -> GroundExpr a -> GroundExpr a

  App :: GroundExpr (a -> b) -> GroundExpr a -> GroundExpr b
  Fun :: Text -> Q (TExp (a -> b)) -> GroundExpr (a -> b)

data Floor a where
  Inj :: Floor a -> Floor b -> Floor (a,b)
  Floor :: Seq Two -> GroundExpr a -> Floor a

--deriving instance Show a => Show (Floor a)

floor :: Expr a -> Floor a
floor = go S.empty . Full.optimizeExpr
  where
    go :: Seq Two -> Expr a -> Floor a
    go to expr = case expr of
      Full.Inj e1 e2 ->
        Inj (go (to |> One) e1) (go (to |> Two) e2)
      e -> Floor to (lowerExpr e)

lowerExpr :: Expr a -> GroundExpr a
lowerExpr = go S.empty
  where
    go :: Seq Two -> Expr a -> GroundExpr a
    go addr expr = case expr of
      Full.Proj1 e1 -> coerceLeft $ go (One <| addr) e1
      Full.Proj2 e1 -> coerceRight $ go (Two <| addr) e1
      Full.Var -> Var addr
      Full.Const c -> Const c
      Full.App e1 e2 -> App (go addr e1) (go addr e2)
      Full.Fun f q -> Fun f q
      Full.If e1 e2 e3 -> If (go addr e1) (go addr e2) (go addr e3)
      _ -> error "did not expect injection at this point"

    coerceLeft :: GroundExpr (a,b) -> GroundExpr a
    coerceLeft (Var v) = Var v
    coerceLeft _ = error "cannot coerce function that produces a tuple"

    coerceRight :: GroundExpr (a,b) -> GroundExpr b
    coerceRight (Var v) = Var v
    coerceRight _ = error "cannot coerce function that produces a tuple"
