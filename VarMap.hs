{-# LANGUAGE TemplateHaskell #-}
module VarMap where

import GOperator(GCell(..))
import Expr(Symbol)

import Data.Label
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Printf

data VarMap = VarMap { _vm_free :: S.Set GCell,
                       _vm_mapping :: M.Map Symbol GCell
                     } deriving Show

mkLabels [''VarMap]

empty_vm = VarMap { _vm_free = S.fromList $ map GCell [1..10], _vm_mapping =  M.empty }

vm_allocate :: Symbol -> Maybe GCell -> VarMap -> VarMap
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
