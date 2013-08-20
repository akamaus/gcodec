{-# LANGUAGE GADTs, TemplateHaskell #-}
module GCode where

import Expr
import GOperator
import VarMap

import Data.Label
import Data.Word
import Text.Printf
import qualified Control.Monad.RWS as RWS
import qualified Data.Set as S

type GCode = RWS.RWS () GOperator GCompileState

data GCompileState = GCS {
  _gsc_vars :: VarMap, -- mapping from symbolic variables to numeric memory cells
  _gsc_ref_labels :: S.Set Label, -- labels referenced from generated code
  _gsc_gen_labels :: S.Set Label  -- already generated labels
  } deriving Show

mkLabels [''GCompileState]

init_cs = GCS { _gsc_vars = empty_vm, _gsc_ref_labels = S.empty, _gsc_gen_labels = S.empty }
gcodeGen gcode = RWS.runRWS gcode () init_cs

allocate :: Maybe GCell -> GCode (Cell t)
allocate mgcell = do
  cs <- RWS.get
  let (GCell n, vm) = (vm_allocate mgcell) $ get gsc_vars cs
  RWS.put $ set gsc_vars vm cs
  return $ Cell n

-- creates a variable with a given name
newVar :: GCode (Cell t)
newVar = allocate Nothing

-- gives a name to a cell
nameCell :: Word -> GCode (Cell t)
nameCell cell_num = allocate (Just $ GCell cell_num)

gIf :: Expr Bool -> GCode () -> GCode () -> GCode ()
gIf = undefined

(#=) :: Cell a -> Expr a -> GCode ()
(#=) = undefined

while :: Expr Bool -> GCode () -> GCode ()
while = undefined

goto :: Label -> GCode ()
goto = undefined
