{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Language.CodeGen.LLVM where

import           Prelude hiding (init)

import           Control.Monad.State

import           Data.Foldable(toList)
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

import           Language.SimpleExpression (SimpleExpr)
import qualified Language.SimpleExpression as Simple
import           Language.Expression (Expr)
import qualified Language.Expression as Full
import           Language.Function

data Two = One | Two

instance Show Two where
  show One = "1"
  show Two = "2"

data GroundExpr a where
  Var :: Seq Two -> GroundExpr Double
  Const :: Double -> GroundExpr Double

  App :: GroundExpr (a -> b) -> GroundExpr a -> GroundExpr b
  Fun :: Fun (a -> b) -> GroundExpr (a -> b)

deriving instance Show (GroundExpr a)

data Assignment where
  Assignment :: Seq Two -> Maybe Double -> GroundExpr Double -> Assignment

deriving instance Show Assignment

type Assignments = Seq Assignment

modul :: String -> Assignments -> Module
modul n ass = L.defaultModule
  { L.moduleName = n
  , L.moduleDefinitions = return $ mainFunction ass
  }

mainFunction :: Assignments ->  Definition
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

mainBody :: Assignments -> [BasicBlock]
mainBody as =
  let cgs = execState (genInstructions as) empty
  in [ L.BasicBlock
        (L.Name "init")
        (toList $ initBlock cgs)
        (L.Do (L.Br (L.Name "loop") []))
     , L.BasicBlock
        (L.Name "loop")
        (toList $ loopBlock cgs)
        (L.Do (L.Br (L.Name "loop") []))
     ]

genInstructions :: Seq Assignment -> Gen ()
genInstructions as = do
  forM_ as $ \(Assignment to init e) -> do
    case init of
      Just c -> initInst $ initName to L.:= L.Add False False (L.ConstantOperand (Constant.Float (Float.Double 0))) (L.ConstantOperand (Constant.Float (Float.Double c))) []
      Nothing -> initInst $ initName to L.:= L.Add False False (L.ConstantOperand (Constant.Float (Float.Double 0))) (L.ConstantOperand (Constant.Float (Float.Double 0))) []
    lastName <- loopExpr e
    loopInst $ loopName to L.:= L.Add False False (L.ConstantOperand (Constant.Float (Float.Double 0))) (L.LocalReference Type.double lastName) []
  where

    loopExpr :: GroundExpr Double -> Gen L.Name
    loopExpr expr = case expr of
      Var n -> do
        let lx = loopName n
            ix = initName n
        loopInst $ lx L.:= L.Phi Type.double [ (L.LocalReference Type.double ix, L.Name "init")
                                             , (L.LocalReference Type.double lx, L.Name "loop")
                                             ] []
        return lx
      Const c -> do
        x <- fresh
        loopInst $ x L.:= L.Add False False (L.ConstantOperand (Constant.Float (Float.Double c))) (L.ConstantOperand (Constant.Float (Float.Double c))) []
        return x
      GFun2 Add e1 e2 -> app2 L.Add e1 e2
      GFun2 Mult e1 e2 -> app2 L.Mul e1 e2
      GFun2 Sub e1 e2 -> app2 L.Sub e1 e2

    app2 :: Op2 -> GroundExpr Double -> GroundExpr Double -> Gen L.Name
    app2 f e1 e2 = do
      x1 <- loopExpr e1
      x2 <- loopExpr e2
      x3 <- fresh
      loopInst $ x3 L.:= f False False (L.LocalReference Type.double x1) (L.LocalReference Type.double x2) []
      return x3

type Op2 = Bool -> Bool -> L.Operand -> L.Operand -> L.InstructionMetadata -> Instruction

initName :: Seq Two -> L.Name
initName ids = L.Name $ 'i' : (concat (toList (fmap show ids)))

loopName :: Seq Two -> L.Name
loopName ids = L.Name $ 'l' : (concat (toList (fmap show ids)))

data CodeGenState = CodeGenState
    { initBlock    :: Seq (Named Instruction)
    , loopBlock    :: Seq (Named Instruction)
    , nextVariable :: Word
    }

empty :: CodeGenState
empty = CodeGenState S.empty S.empty 0

type Gen = State CodeGenState

initInst :: Named Instruction -> Gen ()
initInst inst = modify $ \cgs -> cgs {initBlock = initBlock cgs |> inst}

loopInst :: Named Instruction -> Gen ()
loopInst inst = modify $ \cgs -> cgs {loopBlock = loopBlock cgs |> inst}

fresh :: Gen L.Name
fresh = state (\cgs -> let i = nextVariable cgs in (L.UnName i, cgs {nextVariable = i+1}))

assignments :: SimpleExpr e -> Expr e -> Seq Assignment
assignments prod0 = go S.empty prod0 . Full.optimizeExpr
  where
    go :: Seq Two -> SimpleExpr e -> Expr e -> Seq Assignment
    go to prod expr = case (prod,expr) of
      (Simple.Inj p1 p2,Full.Inj e1 e2) ->
        go (to |> One) p1 e1 >< go (to |> Two) p2 e2
      (Simple.Const c,e1) -> ass to (Just c) e1
      (Simple.Unit,e1) -> ass to Nothing e1
      _ -> error "cannot happen"

    ass to p expr = case lowerExpr expr of
        Just e -> return $ Assignment to p e
        Nothing -> error "Expr is not ground."

pattern Fun1 f e1 = Full.App (Full.Fun f) e1
pattern GFun1 f e1 = App (Fun f) e1
pattern Fun2 f e1 e2 = Full.App (Full.App (Full.Fun f) e1) e2
pattern GFun2 f e1 e2 = App (App (Fun f) e1) e2

lowerExpr :: Expr a -> Maybe (GroundExpr Double)
lowerExpr = go S.empty
  where
    go :: Seq Two -> Expr a -> Maybe (GroundExpr Double)
    go addr expr = case expr of
      Full.Proj1 e1 -> go (One <| addr) e1
      Full.Proj2 e1 -> go (Two <| addr) e1
      Full.Var -> Just $ Var addr
      Full.Const d -> Just $ Const d
      Fun2 Add e1 e2 -> GFun2 Add <$> go addr e1 <*> go addr e2
      Fun2 Mult e1 e2 -> GFun2 Mult <$> go addr e1 <*> go addr e2
      Fun2 Sub e1 e2 -> GFun2 Sub <$> go addr e1 <*> go addr e2
      Fun2 Div e1 e2 -> GFun2 Div <$> go addr e1 <*> go addr e2
      Fun1 Abs e1 -> GFun1 Abs <$> go addr e1
      Fun1 Signum e1 -> GFun1 Signum <$> go addr e1
      Fun1 Sin e1 -> GFun1 Sin <$> go addr e1
      Fun1 Cos e1 -> GFun1 Cos <$> go addr e1
      Full.Fun _ -> Nothing
      Full.Inj _ _ -> Nothing
      Full.App _ _ -> Nothing
