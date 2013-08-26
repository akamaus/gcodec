{-# LANGUAGE GADTs, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module GCode(module GCode,
             Cell, Expr, gRead, (#>)) where

import Expr
import qualified GOperator as G
import GOperator (GOperator)
import VarMap

import Control.Applicative
import Control.Monad
import Data.Label
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

type GCode = RWS.RWS WhileDepth CompileResults GCompileState

-- Helpers for genting warnings, errors and code
warn w = RWS.tell ([w],        RWS.mempty, RWS.mempty)
err e =  RWS.tell (RWS.mempty, [e],        RWS.mempty)
gen c = RWS.tell (RWS.mempty, RWS.mempty, c         )

data GCompileState = GCS {
  _gsc_vars :: VarMap, -- mapping from symbolic variables to numeric memory cells
  _gsc_ref_labels :: S.Set G.Label, -- labels referenced from generated code
  _gsc_gen_labels :: [G.Label]  -- already generated labels
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
local_block :: GCode () -> GCode GOperator
local_block = liftM snd . RWS.censor (\(w,e,c) -> (w,e,RWS.mempty)) . RWS.listens (\(_,_,c) -> c)

-- Validated labels mentioned in generated code
check_labels :: GCode ()
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
    False -> do let label_list = zip (reverse $ get gsc_gen_labels st) (map (G.mkLabel . printf "N%04d") [10 :: Int,20 ..])
                    label_renamer n = fromMaybe (error "PANIC: label renamer can't find a label") $ lookup n label_list
                G.putGOps label_renamer code

-- *****************
--  EDSL primitives
-- *****************

-- Allocates a given cell or any free one
allocate :: Maybe G.GCell -> GCode (Cell t)
allocate mgcell = do
  (c@(G.GCell n), vm) <- (vm_allocate mgcell) <$> L.gets gsc_vars
  L.puts gsc_vars vm
  return $ Cell c

-- Creates a variable with a given name
newVar :: Expr t -> GCode (Cell t)
newVar v0 = do n <- allocate Nothing
               n #= v0
               return n


-- Gives a name to a cell
nameCell :: Word -> GCode (Cell t)
nameCell cell_num = allocate (Just $ G.GCell cell_num)

-- GCode instructions
-- emits If
gIf :: Expr Bool -> GCode () -> GCode ()
gIf pred branch = do
  let gp = eval pred
  code <- saving gsc_vars $ local_block branch
  gen $ G.GIf gp code

-- emits Assignment
(#=) :: Cell a -> Expr a -> GCode ()
(#=) c e = gen $ G.GAssign (unCell c) (eval e)

while :: Expr Bool -> GCode () -> GCode ()
while cond body = do
  depth <- RWS.ask
  when (depth > 3) $ warn $ printf "Generating while of depth %d" depth
  let expr = eval cond
  code <- RWS.local (+1) $ saving gsc_vars $ local_block body
  gen $ G.GWhile depth expr code

-- Generates a goto operator
goto :: String -> GCode ()
goto lbl_str = do
  let lbl = G.mkLabel lbl_str
  L.modify gsc_ref_labels $ S.insert lbl
  gen $ G.GGoto lbl

-- Creates a label at given point
label :: String -> GCode ()
label lbl_str = do
  let lbl = G.mkLabel lbl_str
  labels <- L.gets gsc_gen_labels
  case elem lbl labels of
    False -> do L.puts gsc_gen_labels (lbl:labels)
                gen $ G.GLabel lbl
    True -> error $ printf "labels must be unique, but %s is already defined" lbl_str

frame :: [G.GInstruction] -> GCode ()
frame = gen . G.GFrame

class CInstruction a where
  g :: Int -> a
  m :: Int -> a
  x :: Expr Double -> a
  y :: Expr Double -> a
  z :: Expr Double -> a

instance CInstruction G.GInstruction where
  g = G.G
  m = G.M
  x = G.X . eval
  y = G.Y . eval
  z = G.Z . eval

instance CInstruction (GCode ()) where
  g i = frame [g i]
  m i = frame [m i]
  x expr = frame [x expr]
  y expr = frame [y expr]
  z expr = frame [z expr]
