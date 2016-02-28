{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.CodeGen.TH where

import Prelude hiding (fst,snd)

import Language.Haskell.TH.Syntax
import Language.Expression
import Language.Function
import Language.SynArrow
import Language.SimpleExpression (SimpleExpr,CompressedSimpleExpr(..),Decide(..),merge)
import qualified Language.SimpleExpression as Simple
import Unsafe.Coerce

compile :: SynArrow SimpleExpr (Function f) a b -> Q Exp
compile f = case optimize f of
  LoopD i g -> [| ($(reifySimpleExpr i) , $(reifyFunction i (coerceFun g))) |]
  --Arr g -> unTypeQ [|| $$(reifyExpr g) ||]
  _ -> fail "optimize did not produce normal form"

reifySimpleExpr :: SimpleExpr a -> Q Exp
reifySimpleExpr expr = case Simple.compressSimpleExpr expr of
  Yes expr' -> reifyCompressedSimpleExp expr'
  No -> fail "simple expr could not be compressed"

reifyCompressedSimpleExp :: CompressedSimpleExpr a -> Q Exp
reifyCompressedSimpleExp expr = case expr of
  CInj e1 e2 -> [| ($(reifyCompressedSimpleExp e1),$(reifyCompressedSimpleExp e2) ) |]
  CConst c -> [| c |]

data Pattern a where
  Tuple :: Pattern a -> Pattern b -> Pattern (a,b)
  Variable :: (Maybe Name) -> Name -> Pattern a
deriving instance (Show (Pattern a))

data CompressedPattern a where
  CTuple :: CompressedPattern a -> CompressedPattern b -> CompressedPattern (a,b)
  CVariable :: Name -> Name -> CompressedPattern a

compressPattern :: Pattern a -> Decide CompressedPattern
compressPattern pat = case pat of
  Variable (Just n1) n2 -> Yes (CVariable n1 n2)
  Variable Nothing _ -> No
  Tuple p1 p2 -> merge (\p1' p2' -> Yes (CTuple p1' p2')) (compressPattern p1) (compressPattern p2)

reifyPattern :: SimpleExpr a -> Q (Pattern a)
reifyPattern expr = case expr of
  Simple.Inj e1 e2 -> Tuple <$> reifyPattern e1 <*> reifyPattern e2
  Simple.Fix -> do
    c <- newName "x"
    return $ Variable Nothing c
  _ -> do
    a <- newName "x"
    b <- newName "x"
    return $ Variable (Just a) b

inputPattern :: CompressedPattern a -> Pat
inputPattern pat = case pat of
  CTuple p1 p2 -> TupP $ [inputPattern p1, inputPattern p2]
  CVariable n _ -> VarP n

letPattern :: Pattern a -> Pat
letPattern pat = case pat of
  Tuple p1 p2 -> TupP $ [letPattern p1, letPattern p2]
  Variable _ n -> VarP n

outputExpr :: CompressedPattern a -> Exp
outputExpr pat = case pat of
  CTuple p1 p2 -> TupE [outputExpr p1, outputExpr p2]
  CVariable _ n -> VarE n

reifyFunction :: SimpleExpr c -> Function (a,c) (a,c) (b,c) -> Q Exp
reifyFunction e (Function f) = do
  pat <- reifyPattern e
  a <- newName "x"
  b <- newName "x"
  let pat' = (Tuple (Variable (Just a) b) pat)
  Yes cpat <- return $ compressPattern pat'
  f' <- reifyExpr pat' $ optimizeExpr (f Var)

  return $ LamE [inputPattern cpat]
         $ LetE [ValD (letPattern pat')
                      (NormalB (unType f')) []
                ] (outputExpr cpat)

reifyExpr :: Pattern a -> Expr a b -> Q (TExp b)
reifyExpr = go
  where
    go :: Pattern a -> Expr a b -> Q (TExp b)
    go pat expr = case expr of
        Var -> variableName pat expr
        Proj1 _ -> variableName pat expr
        Proj2 _ -> variableName pat expr
        Inj e1 e2 -> [|| ($$(go pat e1), $$(go pat e2)) ||]
        Const c -> [|| c ||]
        Fun Add -> [|| (+) ||]
        Fun Sub -> [|| (-) ||]
        Fun Mult -> [|| (*) ||]
        Fun Div -> [|| (/) ||]
        Fun Abs -> [|| abs ||]
        Fun Signum -> [|| signum ||]
        Fun Sin -> [|| sin ||]
        Fun Cos -> [|| cos ||]
        App e1 e2 -> [|| $$(go pat e1) $$(go pat e2) ||]
        --_ -> failOnWrongPattern

variableName :: Pattern a -> Expr a b -> Q (TExp b)
variableName pat0 expr0 = case go pat0 expr0 of
  (Variable Nothing n) -> unsafeTExpCoerce (return (VarE n))
  (Variable (Just n) _) -> unsafeTExpCoerce (return (VarE n))
  where

    go :: Pattern a -> Expr a b -> Pattern b
    go pat expr = case expr of
      Proj1 e1 -> fst (go pat e1)
      Proj2 e2 -> snd (go pat e2)
      -- This case would typecheck with dependent pattern matching,
      -- since (Var :: Expr a a)
      Var -> unsafeCoerce pat

    fst :: Pattern (a,b) -> Pattern a
    fst pat = case pat of
      Tuple p1 _ -> p1
      Variable n1 n2 -> Variable n1 n2


    snd :: Pattern (a,b) -> Pattern b
    snd pat = case pat of
      Tuple _ p2 -> p2
      Variable n1 n2 -> Variable n1 n2
