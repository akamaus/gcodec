{-# LANGUAGE TemplateHaskell #-}
module VarMap where

import GCode(GCell(..))
import Expr(Symbol)

import Data.Label
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Printf

data VarMap = VarMap { _vm_free :: S.Set GCell} deriving Show

mkLabels [''VarMap]

empty_vm = VarMap { _vm_free = S.fromList $ map GCell [100..500] }

vm_allocate :: Maybe GCell -> VarMap -> (GCell, VarMap)
vm_allocate mcell vm = let
    c = case mcell of
          Nothing -> case S.toList $ get vm_free vm of
            (c1:_) -> c1
            [] -> error $ "no free cells, can't allocate variable"
          Just needed@(GCell n) -> case S.member needed $ get vm_free vm of
            True -> needed
            False -> error $ printf "Can't assign a name to non-free cell %d" n
        in (c, modify vm_free (S.delete c) vm)
