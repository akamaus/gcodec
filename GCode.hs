{-# LANGUAGE GADTs, TemplateHaskell #-}
module GCode where

import Expr
import GOperator

import Data.Label
import Data.Word
import Text.Printf
import qualified Control.Monad.RWS as RWS
import qualified Data.Map as M
import qualified Data.Set as S

type GCode = RWS.RWS () GOperator GCompileState

data VarMap = VarMap { _vm_free :: S.Set GCell,
                       _vm_mapping :: M.Map Symbol GCell
                     } deriving Show

data GCompileState = GCS {
  _gsc_vars :: VarMap, -- mapping from symbolic variables to numeric memory cells
  _gsc_ref_labels :: S.Set Label, -- labels referenced from generated code
  _gsc_gen_labels :: S.Set Label  -- already generated labels
  } deriving Show

mkLabels [''GCompileState, ''VarMap]

empty_vm = VarMap { _vm_free = S.fromList $ map GCell [1..10], _vm_mapping =  M.empty }
init_cs = GCS { _gsc_vars = empty_vm, _gsc_ref_labels = S.empty, _gsc_gen_labels = S.empty }
gcodeGen gcode = RWS.runRWS gcode () init_cs

vm_allocate :: Symbol -> Maybe (GCell) -> VarMap -> VarMap
vm_allocate sym mcell vm
  | M.notMember sym $ get vm_mapping vm = let
        c = case mcell of
          Nothing -> case S.toList $ get vm_free vm of
            (c1:_) -> c1
            [] -> error $ "no free cells, can't allocate " ++ show sym
          Just needed@(GCell n) -> case S.member needed $ get vm_free vm of
            True -> needed
            False -> error $ printf "Can't assign a name %s to non-free cell %d" (show sym) n

        in modify vm_mapping (M.insert sym c) . modify vm_free (S.delete c) $ vm
  | otherwise = error $ printf "Variable %s already exists, can't allocate" (show sym)

vm_lookup :: Symbol -> VarMap -> Maybe GCell
vm_lookup sym = M.lookup sym . get vm_mapping


gIf :: Expr Bool -> GCode () -> GCode () -> GCode ()
gIf = undefined

(#=) :: Cell a -> Expr a -> GCode ()
(#=) = undefined

while :: Expr Bool -> GCode () -> GCode ()
while = undefined

goto :: Label -> GCode ()
goto = undefined

-- creates a variable with a given name
newVar :: Symbol -> GCode (Cell t)
newVar sym = do RWS.modify $ modify gsc_vars (vm_allocate sym Nothing)
                return $ CellSym sym

-- gives a name to a cell
nameCell :: Symbol -> Word -> GCode (Cell t)
nameCell sym cell_num = do RWS.modify $ modify gsc_vars (vm_allocate sym $ Just $ GCell cell_num)
                           return $ CellSym sym
