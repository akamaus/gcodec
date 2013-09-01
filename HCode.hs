{-# LANGUAGE GADTs, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module HCode(module HCode,
             Cell, Expr, gRead) where

import Expr
import GCode
import VarMap

import qualified AwePrelude as W

import Control.Applicative
import Control.Monad
import Data.Label hiding(mkLabel)
import Data.List
import Data.Maybe
import Data.Word
import Text.Printf
import qualified Control.Monad.RWS as RWS
import qualified Data.Label.PureM as L
import qualified Data.Set as S

type CompileResults = ([Warning],[Error],GOperator)
type Warning = String
type Error = String
type WhileDepth = Int

type HCode = RWS.RWS WhileDepth CompileResults GCompileState

-- Helpers for genting warnings, errors and code
warn w = RWS.tell ([w],        RWS.mempty, RWS.mempty)
err e =  RWS.tell (RWS.mempty, [e],        RWS.mempty)
gen c = RWS.tell (RWS.mempty, RWS.mempty, c         )

data GCompileState = GCS {
  _gsc_vars :: VarMap, -- mapping from symbolic variables to numeric memory cells
  _gsc_ref_labels :: S.Set Label, -- labels referenced from generated code
  _gsc_gen_labels :: [Label]  -- already generated labels
  } deriving Show

mkLabels [''GCompileState]

-- Init compiler state
init_cs = GCS { _gsc_vars = empty_vm, _gsc_ref_labels = S.empty, _gsc_gen_labels = [] }

-- Runs a computation storing a given projection of state
saving l m = do
  st <- L.gets l
  res <- m
  L.puts l st
  return res

-- Generates a code block and returns it
local_block :: HCode () -> HCode GOperator
local_block = liftM snd . RWS.censor (\(w,e,c) -> (w,e,RWS.mempty)) . RWS.listens (\(_,_,c) -> c)

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
gcodeGen gcode = do
  let (_, st, (warns, errs, code)) = RWS.runRWS (gcode >> check_labels) 1 init_cs
  when (not $ null warns) $ printf "Warnings:\n%s\n" $ unlines warns
  case (not $ null errs) of
    True -> printf "Errors:\n%s\n" $ unlines errs
    False -> do let label_list = zip (reverse $ get gsc_gen_labels st) (map (mkLabel . printf "N%04d") [10 :: Int,20 ..])
                    label_renamer n = fromMaybe (error "PANIC: label renamer can't find a label") $ lookup n label_list
                putGOps label_renamer code

-- *****************
--  EDSL primitives
-- *****************

-- Allocates a given cell or any free one
allocate :: Maybe GCell -> HCode (Cell t)
allocate mgcell = do
  (c@(GCell n), vm) <- (vm_allocate mgcell) <$> L.gets gsc_vars
  L.puts gsc_vars vm
  return $ Cell c

-- Creates a variable with a given name
newVar :: ToExpr t => t -> HCode (Cell t)
newVar v0 = do n <- allocate Nothing
               n #= (toExpr v0)
               return n


-- Gives a name to a cell
nameCell :: Word -> HCode (Cell t)
nameCell cell_num = allocate (Just $ GCell cell_num)

-- HCode instructions
-- emits If
gIf :: Expr W.Bool -> HCode () -> HCode ()
gIf pred branch = do
  let gp = eval pred
  code <- saving gsc_vars $ local_block branch
  gen $ GIf gp code

-- emits Assignment
(#=) :: Cell a -> Expr a -> HCode ()
(#=) c e = gen $ GAssign (unCell c) (eval e)

while :: Expr W.Bool -> HCode () -> HCode ()
while cond body = do
  depth <- RWS.ask
  when (depth > 3) $ warn $ printf "Generating while of depth %d" depth
  let expr = eval cond
  code <- RWS.local (+1) $ saving gsc_vars $ local_block body
  gen $ GWhile depth expr code

-- Generates a goto operator
goto :: String -> HCode ()
goto lbl_str = do
  let lbl = mkLabel lbl_str
  L.modify gsc_ref_labels $ S.insert lbl
  gen $ GGoto lbl

-- Creates a label at given point
label :: String -> HCode ()
label lbl_str = do
  let lbl = mkLabel lbl_str
  labels <- L.gets gsc_gen_labels
  case elem lbl labels of
    False -> do L.puts gsc_gen_labels (lbl:labels)
                gen $ GLabel lbl
    True -> error $ printf "labels must be unique, but %s is already defined" lbl_str

frame :: [GInstruction ()] -> HCode ()
frame = gen . GFrame

class CInstruction con where
  g :: Int -> con ()
  m :: Int -> con ()
  s :: Expr Double -> con ()
  f :: Expr Double -> con ()
  d :: Int -> con ()
  h :: Int -> con ()
  x :: Expr Double -> con ()
  y :: Expr Double -> con ()
  z :: Expr Double -> con ()
  i :: Expr Double -> con ()
  j :: Expr Double -> con ()
  k :: Expr Double -> con ()
  r :: Expr Double -> con ()

instance CInstruction GInstruction where
  g = check_diap 0 199 .  GInstrI 'G'
  m = check_diap 0 99  .  GInstrI 'M'
  s = GInstrE 'S' . eval
  f = GInstrE 'F' . eval
  d = check_diap 0 1000 . GInstrI 'D'
  h = check_diap 0 1000 . GInstrI 'H'
  x = GInstrE 'X' . eval
  y = GInstrE 'Y' . eval
  z = GInstrE 'Z' . eval
  i = GInstrE 'I' . eval
  j = GInstrE 'J' . eval
  k = GInstrE 'K' . eval
  r = GInstrE 'R' . eval

check_diap a b instr@(GInstrI c k) | k < a && k > b = error $ printf "Code %c must be in diapason %d-%d, but %d given" c a b k
                                   | otherwise = instr

instance CInstruction HCode where
  g i = frame [g i]
  m i = frame [m i]
  s expr = frame [s expr]
  f expr = frame [f expr]
  d i = frame [d i]
  h i = frame [h i]
  x expr = frame [x expr]
  y expr = frame [y expr]
  z expr = frame [z expr]
  i expr = frame [i expr]
  j expr = frame [j expr]
  k expr = frame [k expr]
  r expr = frame [r expr]
