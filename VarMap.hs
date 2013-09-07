{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module VarMap(VarMap, empty_vm,
              VarRequest(..), vm_allocate) where

import GCode(GCell(..))
import Expr(Symbol, Cell(..))

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Category
import Control.Monad.State(MonadState)

import Data.Label
import Data.Label.PureM

import Text.Printf

import qualified Data.Map as M
import qualified Data.Set as S

type VarBank = S.Set GCell

data VarMap = VarMap { _vm_free :: VarBank, _vm_system :: VarBank} deriving Show

data VarRequest = VR_FreeCommon | VR_System GCell -- a request for variable allocation

mkLabels [''VarMap]

empty_vm = VarMap { _vm_free = S.fromList $ map GCell [100..200]
                  , _vm_system = S.fromList $ map GCell [2000..10000]}

vm_allocate :: (MonadState s m, Functor m) => (s :-> VarMap) -> VarRequest -> m (Cell a)
vm_allocate vm_lens req = Cell <$> case req of
  VR_FreeCommon ->  modifyAndGet (vm_free . vm_lens)   $ \bank -> bank_allocate Nothing bank
  VR_System cell -> modifyAndGet (vm_system . vm_lens) $ \bank -> bank_allocate (Just cell) bank

bank_allocate :: Maybe GCell -> VarBank -> (GCell, VarBank)
bank_allocate mcell bank = let
    c = case mcell of
          Nothing -> case S.toList bank of
            (c1:_) -> c1
            [] -> error $ "no free cells, can't allocate a variable"
          Just needed@(GCell n) -> case S.member needed bank of
            True -> needed
            False -> error $ printf "Can't assign a name to non-free cell %d" n
    in (c, S.delete c bank)
