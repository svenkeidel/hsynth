{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.CodeGen.TH where

import Prelude hiding (fst,snd)

import Language.Haskell.TH.Syntax
import Language.Expression
import Language.Function
import Language.SynArrow
import Language.SimpleExpression (SimpleExpr)
import qualified Language.SimpleExpression as Simple

compile :: SynArrow SimpleExpr Function b Double -> Q Exp
compile f = case optimize f of
  LoopD i g -> fixFunction i g
  --Arr g -> unTypeQ [|| $$(reifyExpr g) ||]
  _ -> fail "optimize did not produce normal form"

data Pattern where
  Tuple :: Pattern -> Pattern -> Pattern
  Variable :: (Maybe Name) -> Name -> Name -> Pattern
deriving instance (Show Pattern)

reifyPattern :: SimpleExpr a -> Q Pattern
reifyPattern expr = case expr of
  Simple.Inj e1 e2 -> Tuple <$> reifyPattern e1 <*> reifyPattern e2
  Simple.Fix -> do
    c <- newName "x"
    return $ Variable Nothing c c
  _ -> do
    a <- newName "x"
    b <- newName "x"
    return $ Variable (Just a) b a

inputPattern :: Pattern -> Pat
inputPattern pat = case pat of
  Tuple p1 p2 -> TupP $ [inputPattern p1, inputPattern p2]
  Variable (Just n) _ _ -> VarP n
  Variable Nothing _ _ -> WildP

letPattern :: Pattern -> Pat
letPattern pat = case pat of
  Tuple p1 p2 -> TupP $ [letPattern p1, letPattern p2]
  Variable _ n _ -> VarP n

outputExpr :: Pattern -> Exp
outputExpr pat = case pat of
  Tuple p1 p2 -> TupE [outputExpr p1, outputExpr p2]
  Variable _ n _ -> VarE n

argument :: Pattern -> Exp
argument pat = case pat of
  Tuple p1 p2 -> TupE [argument p1, argument p2]
  Variable _ _ n -> VarE n

fixFunction :: SimpleExpr c -> Function (a,c) (b,c) -> Q Exp
fixFunction e (Function f) = do
  pat <- reifyPattern e
  a <- newName "x"
  b <- newName "x"
  f' <- reifyFunction (Tuple (Variable (Just a) b a) pat) $ optimizeExpr (f Var)
  return $ LamE [inputPattern pat]
         $ LetE [ValD (letPattern pat)
                      (NormalB (AppE f' (argument pat))) []
                ] (outputExpr pat)

reifyFunction :: Pattern -> Expr a -> Q Exp
reifyFunction = go
  where
    go :: Pattern -> Expr b -> Q Exp --(TExp b)
    go pat expr = case expr of
        Var -> fromMaybe $ variableName pat expr
        Proj1 _ -> fromMaybe $ variableName pat expr
        Proj2 _ -> fromMaybe $ variableName pat expr
        Inj e1 e2 -> [| ($(go pat e1), $(go pat e2)) |]
        Const c -> [| c |]
        Fun Add -> [| (+) |]
        Fun Sub -> [| (-) |]
        Fun Mult -> [| (*) |]
        Fun Div -> [| (/) |]
        Fun Abs -> [| abs |]
        Fun Signum -> [| signum |]
        Fun Sin -> [| sin |]
        Fun Cos -> [| cos |]
        App e1 e2 -> [| $(go pat e1) $(go pat e2) |]
        --_ -> failOnWrongPattern
      where
        fromMaybe (Just a) = return a
        fromMaybe Nothing = failOnWrongPattern
        failOnWrongPattern =
            fail $ "unexpected pattern during code generation " ++ show (pat,expr)



variableName :: Pattern -> Expr b -> Maybe Exp
variableName pat0 expr0 = case go pat0 expr0 of
  Just (Variable _ _ n) -> Just (VarE n)
  _ -> Nothing
  where
    go :: Pattern -> Expr b -> Maybe Pattern
    go pat expr = case (pat,expr) of
      (_,Proj1 e1) -> fst <$> go pat e1
      (_,Proj2 e2) -> snd <$> go pat e2
      (Variable {},Var) -> Just pat
      _                 -> Nothing

fst :: Pattern -> Pattern
fst pat = case pat of
  Tuple p1 _ -> p1
  Variable n1 n2 n3 -> Variable n1 n2 n3


snd :: Pattern -> Pattern
snd pat = case pat of
  Tuple _ p2 -> p2
  Variable n1 n2 n3 -> Variable n1 n2 n3
