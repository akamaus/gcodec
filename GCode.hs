{-# LANGUAGE GADTs, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module GCode where

import Expr
import qualified GOperator as G
import GOperator (GOperator)
import VarMap

import Data.Label
import Data.Word
import Text.Printf
import qualified Control.Monad.RWS as RWS
import qualified Data.Set as S

type GCode = RWS.RWS () GOperator GCompileState

data GCompileState = GCS {
  _gsc_vars :: VarMap, -- mapping from symbolic variables to numeric memory cells
  _gsc_ref_labels :: S.Set G.Label, -- labels referenced from generated code
  _gsc_gen_labels :: S.Set G.Label  -- already generated labels
  } deriving Show

mkLabels [''GCompileState]

init_cs = GCS { _gsc_vars = empty_vm, _gsc_ref_labels = S.empty, _gsc_gen_labels = S.empty }
gcodeGen gcode = RWS.runRWS gcode () init_cs

allocate :: Maybe G.GCell -> GCode (Cell t)
allocate mgcell = do
  cs <- RWS.get
  let (G.GCell n, vm) = (vm_allocate mgcell) $ get gsc_vars cs
  RWS.put $ set gsc_vars vm cs
  return $ Cell n

-- creates a variable with a given name
newVar :: GCode (Cell t)
newVar = allocate Nothing

-- gives a name to a cell
nameCell :: Word -> GCode (Cell t)
nameCell cell_num = allocate (Just $ G.GCell cell_num)

gIf :: Expr Bool -> GCode () -> GCode () -> GCode ()
gIf = undefined

(#=) :: Cell a -> Expr a -> GCode ()
(#=) = undefined

while :: Expr Bool -> GCode () -> GCode ()
while = undefined

goto :: G.Label -> GCode ()
goto = undefined

label :: GCode G.Label
label = undefined

frame :: [Instruction] -> GCode ()
frame = undefined

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