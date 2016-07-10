{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
module Language.GroundExpr(GroundExpr(..),Floor(..),floor,Two(..)) where

import           Prelude hiding (floor)

import           Data.Sequence (Seq,(<|),(|>))
import qualified Data.Sequence as S
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Constant
import           Language.Expression (Expr)
import qualified Language.Expression as Full
import           Language.Haskell.TH.Syntax

data Two = One | Two
  deriving (Eq,Ord)

instance Show Two where
  show One = "1"
  show Two = "2"

data GroundExpr a where
  Var :: Seq Two -> GroundExpr a
  Const :: Constant a => a -> GroundExpr a

  If :: GroundExpr Bool -> GroundExpr a -> GroundExpr a -> GroundExpr a

  App :: GroundExpr (a -> b) -> GroundExpr a -> GroundExpr b
  Fun :: Text -> Q (TExp (a -> b)) -> GroundExpr (a -> b)

-- deriving instance Show (GroundExpr a)
instance Show (GroundExpr a) where
  showsPrec d e = case e of
    Var v -> showsPrec d v
    Const a -> showsPrec d a
    If e1 e2 e3 -> showParen (d > appPrec) $
      showString "if " .
      showsPrec (appPrec+1) e1 .
      showString " " .
      showsPrec (appPrec+1) e2 .
      showString " " .
      showsPrec (appPrec+1) e3
    App e1 e2 -> showParen (d > appPrec) $
      showsPrec (appPrec+1) e1 .
      showString " " .
      showsPrec (appPrec+1) e2
    Fun t _ -> showString (T.unpack t)
    where
       appPrec = 10

data Floor a where
  Inj :: Floor a -> Floor b -> Floor (a,b)
  Floor :: Seq Two -> GroundExpr a -> Floor a

instance Show (Floor a) where
  showsPrec d e = case e of
   Inj e1 e2 ->
     showString "(" .
     showsPrec appPrec e1 .
     showString "," .
     showsPrec appPrec e2 .
     showString ")"
   Floor x e1 -> showParen (d > appPrec) $
     showsPrec (appPrec+1) x .
     showString " <- " .
     showsPrec (appPrec+1) e1
   where
       appPrec = 10 :: Int

floor :: Expr a -> Floor a
floor = go S.empty . Full.optimizeExpr
  where
    go :: Seq Two -> Expr a -> Floor a
    go to expr = case expr of
      Full.Inj e1 e2 ->
        Inj (go (to |> One) e1) (go (to |> Two) e2)
      e -> Floor to (lowerExpr e)

lowerExpr :: Expr a -> GroundExpr a
lowerExpr e0 = go S.empty e0
  where
    go :: Seq Two -> Expr a -> GroundExpr a
    go addr expr = case expr of
      Full.Proj1 e1 -> coerceLeft $ go (One <| addr) e1
      Full.Proj2 e1 -> coerceRight $ go (Two <| addr) e1
      Full.Var -> Var addr
      Full.Const c -> Const c
      Full.App e1 e2 -> App (go addr e1) (go addr e2)
      Full.Fun n f -> Fun n f
      Full.If e1 e2 e3 -> If (go addr e1) (go addr e2) (go addr e3)
      Full.Inj {} -> error $ "did not expect injection at this point: " ++ show expr ++ " in " ++ show e0

    coerceLeft :: GroundExpr (a,b) -> GroundExpr a
    coerceLeft (Var v) = Var v
    coerceLeft e = error $ "cannot coerce function that produces a tuple" ++ show e

    coerceRight :: GroundExpr (a,b) -> GroundExpr b
    coerceRight (Var v) = Var v
    coerceRight e = error $ "cannot coerce function that produces a tuple" ++ show e
