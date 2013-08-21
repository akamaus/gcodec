{-# LANGUAGE GADTs #-}
module Expr where

import qualified Data.ByteString as S
import Data.Word

type Symbol = S.ByteString
data Cell t = Cell Word deriving (Eq, Ord, Show)

gRead :: Cell t -> Expr t
gRead = Read

data Expr t where
  BoolE :: Bool -> Expr Bool
  NumE :: Num t => t -> Expr t
  Add :: Num t => Expr t -> Expr t -> Expr t
  Sub :: Num t => Expr t -> Expr t -> Expr t
  And :: Expr Bool -> Expr Bool -> Expr Bool
  Not :: Expr Bool -> Expr Bool
  Eq :: Eq t => Expr t -> Expr t -> Expr Bool
  Gt :: Ord t => Expr t -> Expr t -> Expr Bool
  Read :: Cell t -> Expr t

instance Num t => Num (Expr t) where
  fromInteger = NumE . fromInteger
  (+) = Add
  (-) = Sub

(#==) :: Eq t => Expr t -> Expr t -> Expr Bool
e1 #== e2 = Eq e1 e2

(#>) :: Ord t => Expr t -> Expr t -> Expr Bool
e1 #> e2 = Gt e1 e2
