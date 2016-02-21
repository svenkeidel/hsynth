{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Language.CodeGen.TH where

import Language.Haskell.TH.Syntax
import Language.Expression
import Language.Function
import Language.SynArrow
import Language.SimpleExpression (SimpleExpr)
import qualified Language.SimpleExpression as Simple

compile :: SynArrow SimpleExpr Function b Double -> Q Exp
compile f = case optimize f of
  LoopD i (Arr g) -> unTypeQ [|| ($$(reifySimpleExpr i), $$(reifyExpr g)) ||]
  Arr g -> unTypeQ [|| $$(reifyExpr g) ||]
  _ -> fail "optimize did not produce normal form"

reifySimpleExpr :: SimpleExpr a -> Q (TExp a)
reifySimpleExpr expr = case expr of
  Simple.Inj e1 e2 -> [|| ($$(reifySimpleExpr e1), $$(reifySimpleExpr e2)) ||]
  Simple.Unit -> [|| () ||]
  Simple.Const c -> [|| c ||]

reifyExpr :: Function a b -> Q (TExp (a -> b))
reifyExpr (Function f) = [|| \x -> $$( go (f Var) [| x |]) ||]
  where
    go :: Expr b -> Q Exp -> Q (TExp b)
    go expr x =
      case expr of
        Var -> unsafeTExpCoerce x
        Inj e1 e2 -> [|| ($$(go e1 x), $$(go e2 x)) ||]
        Proj1 e1 -> [|| fst $$(go e1 x) ||]
        Proj2 e2 -> [|| snd $$(go e2 x) ||]
        Const c -> [|| c ||]
        Fun Add -> [|| (+) ||]
        Fun Sub -> [|| (-) ||]
        Fun Mult -> [|| (*) ||]
        Fun Div -> [|| (/) ||]
        Fun Abs -> [|| abs ||]
        Fun Signum -> [|| signum ||]
        Fun Sin -> [|| sin ||]
        Fun Cos -> [|| cos ||]
        App e1 e2 -> [|| $$(go e1 x) $$(go e2 x) ||]
