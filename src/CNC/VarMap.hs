{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module CNC.VarMap(VarMap, empty_vm,
              VarRequest(..), vm_allocate) where

import CNC.FanucMacro(FCell(..))
import CNC.Expr(Symbol, Cell(..))

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Category
import Control.Monad.State(MonadState)

import Data.Label
import Data.Label.PureM

import Text.Printf

import qualified Data.Map as M
import qualified Data.Set as S

type VarBank = S.Set FCell

data VarMap = VarMap { _vm_free :: VarBank, _vm_system :: VarBank} deriving Show

data VarRequest = VR_FreeCommon | VR_System FCell -- a request for variable allocation

mkLabels [''VarMap]

empty_vm = VarMap { _vm_free = S.fromList $ map FCell [100..200]
                  , _vm_system = S.fromList $ map FCell [2000..10000]}

vm_allocate :: (MonadState s m, Functor m) => (s :-> VarMap) -> VarRequest -> m (Cell a)
vm_allocate vm_lens req = Cell <$> case req of
  VR_FreeCommon ->  modifyAndGet (vm_free . vm_lens)   $ \bank -> bank_allocate_free bank
  VR_System cell -> modifyAndGet (vm_system . vm_lens) $ \bank -> bank_allocate_system cell bank

bank_allocate_free :: VarBank -> (FCell, VarBank)
bank_allocate_free bank = case S.toList bank of
  (c:_) -> (c, S.delete c bank)
  [] -> error $ "no free cells, can't allocate a variable"

bank_allocate_system :: FCell -> VarBank -> (FCell, VarBank)
bank_allocate_system cell@(FCell n) bank = case S.member cell bank of
  True -> (cell, bank)
  False -> error $ printf "Can't assign a name to non-existent system cell %d" n
