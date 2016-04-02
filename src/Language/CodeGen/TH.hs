{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.CodeGen.TH where

import           Prelude hiding (fst,snd)

import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Map (Map)

import           Language.Haskell.TH.Syntax hiding (lookupName)
import           Language.Haskell.TH.Ppr
import           Language.Expression
import           Language.SynArrow
import           Language.SimpleExpression (SimpleExpr,CompressedSimpleExpr(..),Decide(..),merge)
import qualified Language.SimpleExpression as Simple
import           Language.GroundExpr (GroundExpr,Floor,Two(..))
import qualified Language.GroundExpr as G

compile :: SynArrow SimpleExpr Function a b -> Q Exp
compile f = case optimize f of
  LoopD i g -> [| ($(reifySimpleExpr i) , $(reifyFunction i g)) |]
  --Arr g -> unTypeQ [|| $$(reifyExpr g) ||]
  g -> fail $ "optimize did not produce normal form: " ++ show g

abstractSyntax :: SynArrow SimpleExpr Function a b -> IO ()
abstractSyntax f = case optimize f of
  LoopD _ (Function g) -> print (optimizeExpr (g Var))
  g -> error $ "optimize did not produce normal form: " ++ show g

prettyPrint :: Q Exp -> IO ()
prettyPrint expr = do
  e <- runQ expr
  putStrLn $ pprint e

reifySimpleExpr :: SimpleExpr a -> Q Exp
reifySimpleExpr expr = case Simple.compressSimpleExpr expr of
  Yes expr' -> reifyCompressedSimpleExp expr'
  No -> fail "simple expr could not be compressed"

reifyCompressedSimpleExp :: CompressedSimpleExpr a -> Q Exp
reifyCompressedSimpleExp expr = case expr of
  CInj e1 e2 -> [| ($(reifyCompressedSimpleExp e1),$(reifyCompressedSimpleExp e2) ) |]
  CConst c -> [| c |]
  CUnit -> [| () |]

data Pattern a where
  Tuple :: Pattern a -> Pattern b -> Pattern (a,b)
  Variable :: (Maybe Name) -> Name -> Pattern a
deriving instance (Show (Pattern a))

data CompressedPattern a where
  CTuple :: CompressedPattern a -> CompressedPattern b -> CompressedPattern (a,b)
  CVariable :: Name -> Name -> CompressedPattern a
deriving instance (Show (CompressedPattern a))

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

outputExpr :: Pattern a -> Exp
outputExpr pat = case pat of
  Tuple p1 p2 -> TupE [outputExpr p1, outputExpr p2]
  Variable _ n -> VarE n

reifyFunction :: forall a b c. SimpleExpr c -> Function (a,c) (b,c) -> Q Exp
reifyFunction e (Function f) = do
  pat <- reifyPattern e
  a <- newName "x"
  b <- newName "x"
  let pat' = Tuple (Variable (Just a) b) pat :: Pattern (a,c)
  Yes inpat <- return $ compressPattern pat'
  f' <- reifyFloor pat' $ G.floor $ f Var
  return $ LamE [inputPattern inpat]
         $ LetE [ValD (letPattern pat')
                      (NormalB (unType f')) []
                ] (outputExpr pat')

type Names = Map (Seq Two) Name

reifyFloor :: Pattern a -> Floor b -> Q (TExp b)
reifyFloor pat flr = case flr of
  G.Inj f1 f2 -> [|| ( $$(reifyFloor pat f1), $$(reifyFloor pat f2) ) ||]
  G.Floor _ e -> reifyExpr pat e

lookupName :: Pattern a -> Seq Two -> Name
lookupName pat name = case (pat,S.viewl name) of
                   (Tuple l _, One S.:< rest) -> lookupName l rest
                   (Tuple _ r, Two S.:< rest) -> lookupName r rest
                   (Variable Nothing n, _) -> n
                   (Variable (Just n) _, _) -> n
                   (Tuple{}, S.EmptyL) -> error (show pat)

reifyExpr :: Pattern a -> GroundExpr b -> Q (TExp b)
reifyExpr pat = go
  where
    go :: GroundExpr a -> Q (TExp a)
    go expr = case expr of
      G.Var n -> unsafeTExpCoerce (return (VarE (lookupName pat n)))
      G.Const c -> [|| c ||]
      G.Fun _ q -> q
      G.App e1 e2 -> [|| $$(go e1) $$(go e2) ||]
