{-# LANGUAGE GADTs, FlexibleInstances #-}
module Expr where

import GOperator

import qualified Data.ByteString as S
import Data.Word

type Symbol = S.ByteString
data Cell t = Cell Word deriving (Eq, Ord, Show)

gRead :: Cell t -> Expr t
gRead = Read

data Expr t where
  BoolE :: Bool -> Expr Bool
  NumE :: (Num t, RealFrac t) => t -> Expr t
  Add :: Num t => Expr t -> Expr t -> Expr t
  Sub :: Num t => Expr t -> Expr t -> Expr t
  And :: Expr Bool -> Expr Bool -> Expr Bool
  Not :: Expr Bool -> Expr Bool
  Eq :: Eq t => Expr t -> Expr t -> Expr Bool
  Gt :: Ord t => Expr t -> Expr t -> Expr Bool
  Read :: Cell t -> Expr t

instance (Num t, RealFrac t) => Num (Expr t) where
  fromInteger = NumE . fromInteger
  (+) = Add
  (-) = Sub

(#==) :: Eq t => Expr t -> Expr t -> Expr Bool
e1 #== e2 = Eq e1 e2

(#>) :: Ord t => Expr t -> Expr t -> Expr Bool
e1 #> e2 = Gt e1 e2

class LoseType e where
  descend :: e -> GExpr

instance LoseType (Expr Bool) where
  descend (BoolE b) = G_Int . fromEnum $ b

instance (Num a, RealFrac a) => LoseType (Expr a) where
  descend (NumE n) = case fromIntegral (round n) == n of
    True -> G_Int $ round n
    False -> G_Float $ realToFrac n

instance LoseType (Expr a) where
  descend (Add e1 e2) = G_Add (descend e1) (descend e2)