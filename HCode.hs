{-# LANGUAGE GADTs, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module HCode(module HCode,
             Cell, Expr, gRead, fix, fup, fi) where

import Expr
import FanucMacro
import VarMap

import Prelude hiding(break)
import qualified AwePrelude as W

import Control.Applicative
import Control.Monad
import Data.Label hiding(mkLabel)
import Data.List
import Data.Maybe
import Data.Word
import System.IO
import Text.Printf
import qualified Control.Monad.RWS as RWS
import qualified Data.Label.PureM as L
import qualified Data.Set as S

type CompileResults = ([Warning],[Error],FOperator)
type Warning = String
type Error = String

type HCode = RWS.RWS CompileEnv CompileResults FCompileState

data FCompileState = FCS {
  _gsc_vars :: VarMap, -- mapping from symbolic variables to numeric memory cells
  _gsc_ref_labels :: S.Set Label, -- labels referenced from generated code
  _gsc_gen_labels :: [Label],  -- already generated labels
  _gsc_fresh_label :: Label,
  _gsc_acc :: Maybe FOperator  -- operator currently being assembled
  } deriving Show

data CompileEnv = CompileEnv { _ce_ext_labels :: [Label], _ce_gwhile_depth :: Int }

mkLabels [''CompileEnv, ''FCompileState]

-- Helpers for generating warnings, errors and code
warn w = RWS.tell ([w],        RWS.mempty, RWS.mempty)
err e =  RWS.tell (RWS.mempty, [e],        RWS.mempty)
gen op = do genAccumulated
            L.puts gsc_acc (Just op)

-- Freezing accumulated operator
genAccumulated :: HCode ()
genAccumulated = do
  acc <- L.gets gsc_acc
  case acc of
    Nothing -> return ()
    Just op -> RWS.tell (RWS.mempty, RWS.mempty, op)
  L.puts gsc_acc Nothing


-- Init compiler state
init_cs = FCS { _gsc_vars = empty_vm, _gsc_ref_labels = S.empty, _gsc_gen_labels = [], _gsc_fresh_label = AutoLabel 1, _gsc_acc = Nothing }
init_ce = CompileEnv { _ce_ext_labels = [], _ce_gwhile_depth = 1 }

-- Runs a computation storing a given projection of state
saving l m = do
  st <- L.gets l
  res <- m
  L.puts l st
  return res

-- Fenerates a code block and returns it
local_block :: HCode () -> HCode FOperator
local_block block = do genAccumulated
                       liftM snd . RWS.censor (\(w,e,c) -> (w,e,RWS.mempty)) . RWS.listens (\(_,_,c) -> c) $ block >> genAccumulated

freshLabel :: HCode Label
freshLabel = L.modifyAndGet (gsc_fresh_label) $ \fl@(AutoLabel k) -> (fl, AutoLabel $ k + 1)

-- registers label in list of used labels
refLabel :: Label -> HCode ()
refLabel lbl = L.modify gsc_ref_labels $ S.insert lbl

-- generates a label, registers it in list of existing labels
genLabel :: Label -> HCode ()
genLabel lbl = do
  labels <- L.gets gsc_gen_labels
  case elem lbl labels of
    False -> do L.puts gsc_gen_labels (lbl:labels)
                gen $ FLabel lbl
    True -> error $ printf "labels must be unique, but %s is already defined" (show lbl)

-- Validated labels mentioned in generated code
check_labels :: HCode ()
check_labels = do
  ref <- L.gets gsc_ref_labels
  gen <- S.fromList <$> L.gets gsc_gen_labels
  let unused_lbls = S.difference gen ref
      unknown_lbls = S.difference ref gen
  mapM_ (\lbl -> warn $ printf "Unused label: %s" (show lbl)) (S.toList unused_lbls)
  mapM_ (\lbl -> err $ printf "unknown label: %s" (show lbl)) (S.toList unknown_lbls)

-- Generates a code and prints it on stdout
putHCode :: HCode () -> IO ()
putHCode hcode = do
  gen_res <- fanucGen hcode
  case gen_res of
    Nothing -> return ()
    Just (st, fcode) -> do let label_list = zip (reverse $ get gsc_gen_labels st) (map (printf "%04d") [10 :: Int, 20 ..])
                               label_renamer n = fromMaybe (error "PANIC: label renamer can't find a label") $ lookup n label_list
                               label_printer = LabelPrinter { lp_frame = ('N':) . label_renamer, lp_ref = label_renamer}
                           putFOps label_printer fcode
-- Generates a code and prints errors on stdout
fanucGen :: HCode () -> IO (Maybe (FCompileState, FOperator))
fanucGen hcode = do
  let (_, st, (warns, errs, fcode)) = RWS.runRWS (hcode >> genAccumulated >> check_labels) init_ce init_cs
  when (not $ null warns) $ hPutStrLn stderr $ printf "Warnings:\n%s\n" $ unlines warns
  case (not $ null errs) of
    True -> do hPutStrLn stderr $ printf "Errors:\n%s\n" $ unlines errs
               return Nothing
    False -> return $ Just (st, fcode)

-- A convenient wrapper, transforms HCode to FCode, fails on errors
hcodeToFCode hcode = do
  gen_res <- fanucGen hcode
  case gen_res of
    Nothing -> error "errors detected while compiling"
    Just (st, fcode) -> return fcode

-- *****************
--  EDSL primitives
-- *****************

-- Creates a variable with a given name and initialize it with compile time constant
newVar :: ToExpr t => t -> HCode (Expr t)
newVar v0 = do n <- gRead <$> vm_allocate gsc_vars VR_FreeCommon
               n #= (toExpr v0)
               return n

-- Creates a variable and initializes it with an expression value
newVarE :: Expr t -> HCode (Expr t)
newVarE v0 = do n <- gRead <$> vm_allocate gsc_vars VR_FreeCommon
                n #= v0
                return n

-- Fives a name to a cell
sysVar :: Word -> HCode (Expr t)
sysVar cell_num = gRead <$> vm_allocate gsc_vars (VR_System (FCell cell_num))

-- Declares a system table
sysTable :: String -> HCode (Expr Int -> Expr t)
sysTable name = do let table e = FTable (mkTableName name) (eval e)
                   return $ gRead . Cell . table

-- HCode instructions
-- emits If
gIf :: Expr W.Bool -> HCode () -> HCode ()
gIf pred branch = do
  let gp = eval pred
  code <- saving gsc_vars $ local_block branch
  case code of
    FOps [FGoto lbl] comment -> -- in case we have a simple goto we can put right into the 'if' body
      gen $ FOps [FIf gp lbl] comment
    large_code -> do
      rest_prog <- freshLabel
      branch_lbl <- freshLabel
      gen $ FIf gp branch_lbl
      gen $ FGoto rest_prog
      genLabel $ branch_lbl
      gen $ code
      genLabel rest_prog
      refLabel branch_lbl -- we used both labels so record this fact
      refLabel rest_prog

-- emits Assignment
infixr 1 #=
(#=) :: Expr a -> Expr a -> HCode ()
(#=) (Read c) e = gen $ FAssign (unCell c) (eval e)
(#=) _ e = error "Left side of an assignment must be a variable"

gwhile :: Expr W.Bool -> HCode () -> HCode ()
gwhile cond body = do
  depth <- L.asks ce_gwhile_depth
  when (depth > 3) $ warn $ printf "Generating while of depth %d" depth
  let expr = eval cond
  rest_prog_lbl <- freshLabel
  code <- L.local ce_ext_labels (rest_prog_lbl :) $ L.local ce_gwhile_depth (+1) $ saving gsc_vars $ local_block body
  gen $ FWhile depth expr code
  genLabel rest_prog_lbl
  refLabel rest_prog_lbl

-- Fenerates a while loop, implemented using IF and FOTO, cause WHILE badly interacts with goto
while :: Expr W.Bool -> HCode () -> HCode ()
while pred body = do
  let gp = eval (Not pred)
  rest_prog_lbl <- freshLabel
  while_lbl <- freshLabel
  genLabel while_lbl
  gen $ FIf gp rest_prog_lbl
  code <- L.local ce_ext_labels (rest_prog_lbl :) $ saving gsc_vars $ local_block body
  gen code
  gen $ FGoto while_lbl
  genLabel rest_prog_lbl
  refLabel while_lbl
  refLabel rest_prog_lbl

-- a for loop, like in C
for :: Expr a -> (Expr a -> Expr W.Bool) -> (Expr a -> Expr a) -> (Expr a -> HCode ()) -> HCode ()
for init pred next body = do
  code <- saving gsc_vars $ local_block $ do
    k <- newVarE init
    while (pred k) $ do
      body k
      k #= next k
  gen code

-- Breaks out of innermost loop
break :: HCode ()
break = break_n 1
-- Escape n-levels of loops
break_n :: Int -> HCode ()
break_n n | n < 1 = error "break_n should be called with a positive number, which specifies how many level of blocks to float up"
          | True = do
            escape_labels <- L.asks ce_ext_labels
            case drop (n-1) escape_labels of
              [] -> error $ "can't break " ++ show n ++ if n>1 then " levels" else " level"
              (lbl:_) -> gen $ FGoto lbl

-- Fenerates a goto operator
goto :: String -> HCode ()
goto lbl_str = do
  let lbl = mkULabel lbl_str
  refLabel lbl
  gen $ FGoto lbl

-- Creates a label at given point
label :: String -> HCode ()
label lbl_str = genLabel $ mkULabel lbl_str

frame :: [FInstruction ()] -> HCode ()
frame = gen . FFrame

-- Emits comment
infix 0 #
(#) :: HCode a -> String -> HCode a
code # comment = do
  r <- code
  acc <- L.gets gsc_acc
  case acc of
    Nothing -> warn "no operator to attach comment to"
    Just op -> L.puts gsc_acc $ Just $ FOps [op] comment
  return r

class CInstruction con where
  g :: Expr Int -> con ()
  m :: Int -> con ()
  t :: Expr Int -> con ()
  s :: Expr Double -> con ()
  f :: Expr Double -> con ()
  d :: Expr Int -> con ()
  h :: Expr Int -> con ()
  x :: Expr Double -> con ()
  y :: Expr Double -> con ()
  z :: Expr Double -> con ()
  i :: Expr Double -> con ()
  j :: Expr Double -> con ()
  k :: Expr Double -> con ()
  r :: Expr Double -> con ()
  p :: Expr Double -> con ()
  l :: Int -> con ()

instance CInstruction FInstruction where
  g = FInstrE 'G' . eval
  m = check_diap 0 200 . FInstrI 'M'
  t = FInstrE 'T' . eval
  s = FInstrE 'S' . eval
  f = FInstrE 'F' . eval
  d = FInstrE 'D' . eval
  h = FInstrE 'H' . eval
  x = FInstrE 'X' . eval
  y = FInstrE 'Y' . eval
  z = FInstrE 'Z' . eval
  i = FInstrE 'I' . eval
  j = FInstrE 'J' . eval
  k = FInstrE 'K' . eval
  r = FInstrE 'R' . eval
  p = FInstrE 'P' . eval
  l = FInstrI 'L'

check_diap a b instr@(FInstrI c k) | k < a && k > b = error $ printf "Code %c must be in diapason %d-%d, but %d given" c a b k
                                   | otherwise = instr

instance CInstruction HCode where
  g attr = frame [g attr]
  m attr = frame [m attr]
  t attr = frame [t attr]
  s attr = frame [s attr]
  f attr = frame [f attr]
  d attr = frame [d attr]
  h attr = frame [h attr]
  x attr = frame [x attr]
  y attr = frame [y attr]
  z attr = frame [z attr]
  i attr = frame [i attr]
  j attr = frame [j attr]
  k attr = frame [k attr]
  r attr = frame [r attr]
  p attr = frame [p attr]
  l attr = frame [l attr]
