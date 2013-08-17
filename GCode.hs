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

-- creates a variable with a given name
newVar :: Symbol -> GCode (Cell t)
newVar sym = do RWS.modify $ modify gsc_vars (vm_allocate sym Nothing)
                return $ CellSym sym

-- gives a name to a cell
nameCell :: Symbol -> Word -> GCode (Cell t)
nameCell sym cell_num = do RWS.modify $ modify gsc_vars (vm_allocate sym $ Just $ GCell cell_num)
                           return $ CellSym sym

gIf :: Expr Bool -> GCode () -> GCode () -> GCode ()
gIf = undefined

(#=) :: Cell a -> Expr a -> GCode ()
(#=) = undefined

while :: Expr Bool -> GCode () -> GCode ()
while = undefined

goto :: Label -> GCode ()
goto = undefined
