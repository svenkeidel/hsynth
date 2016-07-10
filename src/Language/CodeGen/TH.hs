{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.CodeGen.TH where

import           Prelude hiding (fst,snd)

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Maybe
import           Data.Monoid

import           Language.Haskell.TH.Syntax hiding (lookupName)
import           Language.Haskell.TH.Ppr
import           Language.Expression
import           Language.SynArrow
import           Language.SimpleExpression (SimpleExpr,CompressedSimpleExpr(..),Decide(..))
import qualified Language.SimpleExpression as Simple
import           Language.Constant
import           Language.GroundExpr (GroundExpr,Floor,Two(..))
import qualified Language.GroundExpr as G

import           Text.Printf

compile :: SynArrow SimpleExpr Function a b -> Q Exp
compile f = case optimize f of
  LoopD i g -> [| ($(reifySimpleExpr i) , $(reifyFunction i g)) |]
  --Arr g -> unTypeQ [|| $$(reifyExpr g) ||]
  g -> fail $ "optimize did not produce normal form: " ++ show g

abstractSyntax :: SynArrow SimpleExpr Function a b -> IO ()
abstractSyntax f = case optimize f of
  LoopD i (Function g) -> print (i, optimizeExpr (g Var))
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
  CConst c -> SigE <$> lift c <*> pure (constantType c)

data Pattern where
  Tuple :: Pattern -> Pattern -> Pattern
  Variable :: Name -> Pattern
deriving instance (Show Pattern)

topPattern :: Floor a -> Q Pattern
topPattern flr = do
  n <- newName "x"
  goFloor flr (Variable n)
  where
    goFloor :: Floor a -> Pattern -> Q Pattern
    goFloor (G.Inj f1 f2) ip = goFloor f1 ip >>= goFloor f2
    goFloor (G.Floor _ gexp) ip = goGExp gexp ip

    goGExp :: GroundExpr a -> Pattern -> Q Pattern
    goGExp (G.Var n) ip = extendInputPattern n ip
    goGExp (G.App e1 e2) ip = goGExp e1 ip >>= goGExp e2
    goGExp (G.If e1 e2 e3) ip = goGExp e1 ip >>= goGExp e2 >>= goGExp e3
    goGExp G.Const {} ip = return ip
    goGExp G.Fun {} ip = return ip
    --goGExp _ ip = return ip

    extendInputPattern :: Seq Two -> Pattern -> Q Pattern
    extendInputPattern name ip = case (S.viewl name,ip) of
      (One S.:< name', Tuple p1 p2) -> Tuple <$> extendInputPattern name' p1 <*> pure p2
      (Two S.:< name', Tuple p1 p2) -> Tuple <$> pure p1 <*> extendInputPattern name' p2
      (_ S.:< _, Variable _) -> do
        n1 <- newName "x"
        n2 <- newName "x"
        extendInputPattern name (Tuple (Variable n1) (Variable n2))
      (S.EmptyL, _) -> return ip

reifyInputPattern :: Pattern -> Set Name -> Pat
reifyInputPattern pat fv = case pat of
  Tuple p1 p2 -> TupP [reifyInputPattern p1 fv, reifyInputPattern p2 fv]
  -- If the name is not used in the expression, do not bind the name
  -- to avoid "unused variable warnings" in genereated code
  Variable n | n `Set.member` fv -> VarP n
             | otherwise -> WildP

bottomPattern :: Floor a -> Q Pattern
bottomPattern expr = case expr of
  G.Inj e1 e2 -> Tuple <$> bottomPattern e1 <*> bottomPattern e2
  _ -> Variable <$> newName "x"

merge :: (a -> a -> Maybe a) -> Maybe a -> Maybe a -> Maybe a
merge f m1 m2 = case (m1,m2) of
 (Nothing,Nothing) -> Nothing
 (Nothing, Just m2') -> Just m2'
 (Just m1', Nothing) -> Just m1'
 (Just m1',Just m2') -> f m1' m2'

reducePattern :: SimpleExpr b -> Pattern -> Pattern
reducePattern s0 pat0 = case pat0 of
    (Tuple p10 p20) -> fromMaybe pat0 $ Tuple p10 <$> go s0 p20
    p -> p
  where
    go :: SimpleExpr a -> Pattern -> Maybe Pattern
    go s p = case (s,p) of
      (Simple.Inj e1 e2,Tuple p1 p2) ->
        merge (\p1' p2' -> Just (Tuple p1' p2')) (go e1 p1) (go e2 p2)
      (Simple.Fix, _) -> Nothing
      (_,pat) -> Just pat

fixPattern :: SimpleExpr b -> Pattern -> Pattern -> Pattern
fixPattern expr0 ip0 op0 = case (ip0, op0) of
  (Tuple ip1 ip2,Tuple _ op2) -> Tuple ip1 (go expr0 ip2 op2)
  p@(_,_) -> error $ "Input and output patterns have to be tuples, but got " ++ show p
  where
    go :: SimpleExpr b -> Pattern -> Pattern -> Pattern
    go expr ip op = case (expr,ip,op) of
      (Simple.Inj e1 e2,Tuple ip1 ip2, Tuple op1 op2) ->
        Tuple (go e1 ip1 op1) (go e2 ip2 op2)
      (Simple.Fix, _, _) -> op
      (_, _, _) -> ip

combine :: Pattern -> Pattern -> Pattern
combine p10 p20 = case (p10,p20) of
  (Tuple p11 p12,Tuple p21 p22) -> Tuple (combine p11 p21) (combine p12 p22)
  (Variable {}, Tuple {}) -> p20
  (Tuple {}, Variable {}) -> p10
  (Variable {}, Variable {}) -> p10

reifyLetPattern :: Pattern -> Pat
reifyLetPattern pat = case pat of
  Tuple p1 p2 -> TupP [reifyLetPattern p1, reifyLetPattern p2]
  Variable n -> VarP n

reifyOutputExpr :: Pattern -> Exp
reifyOutputExpr pat = case pat of
  Tuple p1 p2 -> TupE [reifyOutputExpr p1, reifyOutputExpr p2]
  Variable n -> VarE n

reifyFunction :: SimpleExpr c -> Function (a,c) (b,c) -> Q Exp
reifyFunction e (Function f) = do
  let flr = G.floor $ f Var
  input <- topPattern flr
  combined <- combine <$> topPattern flr <*> bottomPattern flr
  f' <- reifyFloor (fixPattern e input combined) flr
  return $ LamE [reifyInputPattern (reducePattern e input) (freeVars input flr)]
         $ LetE [ValD (reifyLetPattern combined)
                      (NormalB f') []
                ] (reifyOutputExpr (reducePattern e combined))

freeVars :: Pattern -> Floor b -> Set Name
freeVars pat flr = case flr of
  G.Inj f1 f2 -> freeVars pat f1 <> freeVars pat f2
  G.Floor _ e -> go e
  where
    go :: GroundExpr a -> Set Name
    go expr = case expr of
      G.Var n -> Set.singleton (lookupName pat n)
      G.Const {} -> Set.empty
      G.Fun {} -> Set.empty
      G.App e1 e2 -> go e1 <> go e2
      G.If e1 e2 e3 -> go e1 <> go e2 <> go e3

reifyFloor :: Pattern -> Floor b -> Q Exp
reifyFloor pat flr = case flr of
  G.Inj f1 f2 -> [| ( $(reifyFloor pat f1), $(reifyFloor pat f2) ) |]
  G.Floor _ e -> reifyExpr pat e

reifyExpr :: Pattern -> GroundExpr b -> Q Exp
reifyExpr pat = go
  where
    go :: GroundExpr a -> Q Exp
    go expr = case expr of
      G.Var n -> return (VarE (lookupName pat n))
      G.Const c -> SigE <$> lift c <*> pure (constantType c)
      G.Fun _ q -> unType <$> q
      G.App e1 e2 -> [| $(go e1) $(go e2) |]
      G.If e1 e2 e3 -> [| if $(go e1) then $(go e2) else $(go e3) |]

iff :: Bool -> a -> a -> a
iff x e1 e2 = if x then e1 else e2
{-# INLINE iff #-}

lookupName :: Pattern -> Seq Two -> Name
lookupName pat name = case (pat,S.viewl name) of
                   (Tuple l _, One S.:< rest) -> lookupName l rest
                   (Tuple _ r, Two S.:< rest) -> lookupName r rest
                   (Variable n, _) -> n
                   (Tuple{}, S.EmptyL) -> error $ printf "lookupName failed on pattern %s" (show pat)
