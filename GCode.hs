{-# LANGUAGE GADTs, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module GCode where

import Expr
import qualified GOperator as G
import GOperator (GOperator)
import VarMap

import Control.Applicative
import Data.Label
import qualified Data.Label.PureM as L
import Data.Word
import Text.Printf
import qualified Control.Monad.RWS as RWS
import qualified Data.Set as S
import Control.Monad

type GCode = RWS.RWS () GOperator GCompileState

data GCompileState = GCS {
  _gsc_vars :: VarMap, -- mapping from symbolic variables to numeric memory cells
  _gsc_ref_labels :: S.Set G.Label, -- labels referenced from generated code
  _gsc_gen_labels :: S.Set G.Label  -- already generated labels
  } deriving Show

mkLabels [''GCompileState]

init_cs = GCS { _gsc_vars = empty_vm, _gsc_ref_labels = S.empty, _gsc_gen_labels = S.empty }

-- Generate and print the code
gcodeGen gcode = do
  let (ret, st, code) = RWS.runRWS gcode () init_cs
      ref = get gsc_ref_labels st
      gen = get gsc_gen_labels st
      unused_lbls = S.difference gen ref
      unknown_lbls = S.difference ref gen
  when (not $ S.null unused_lbls) $ printf "Warning, unused labels: %s\n\n" (show $ S.toList unused_lbls)
  case (not $ S.null unknown_lbls) of
    True -> printf "Error, unknown labels: %s\n" (show $ S.toList unknown_lbls)
    False -> G.putGOps code

allocate :: Maybe G.GCell -> GCode (Cell t)
allocate mgcell = do
  (G.GCell n, vm) <- (vm_allocate mgcell) <$> L.gets gsc_vars
  L.puts gsc_vars vm
  return $ Cell n

-- creates a variable with a given name
newVar :: GCode (Cell t)
newVar = allocate Nothing

-- gives a name to a cell
nameCell :: Word -> GCode (Cell t)
nameCell cell_num = allocate (Just $ G.GCell cell_num)

gIf :: Expr Bool -> GCode () -> GCode () -> GCode ()
gIf = error "gIf undefined"

(#=) :: Cell a -> Expr a -> GCode ()
(#=) = error "#= undefined"

while :: Expr Bool -> GCode () -> GCode ()
while = error "while undefined"

-- Generates a goto operator
goto :: String -> GCode ()
goto lbl_str = do
  let lbl = G.mkLabel lbl_str
  L.modify gsc_ref_labels $ S.insert lbl
  RWS.tell $ G.GGoto lbl

-- Creates a label at given point
label :: String -> GCode ()
label lbl_str = do
  let lbl = G.mkLabel lbl_str
  labels <- L.gets gsc_gen_labels
  case S.member lbl labels of
    False -> do L.puts gsc_gen_labels $ S.insert lbl labels
                RWS.tell $ G.GLabel lbl
    True -> error $ printf "labels must be unique, but %s is already defined" lbl_str

frame :: [Instruction] -> GCode ()
frame = error "frame undefined"

data Instruction = G Int | M Int | X (Expr Double) | Y (Expr Double) | Z (Expr Double)

class CInstruction a where
  fG :: Int -> a
  fM :: Int -> a
  fX :: Expr Double -> a
  fY :: Expr Double -> a
  fZ :: Expr Double -> a

instance CInstruction Instruction where
  fG = G
  fM = M
  fX = X
  fY = Y
  fZ = Z

instance CInstruction (GCode ()) where
  fG i = frame [fG i]
  fM i = frame [fM i]
  fX expr = frame [fX expr]
  fY expr = frame [fY expr]
  fZ expr = frame [fZ expr]