{-# LANGUAGE GADTs, FlexibleInstances #-}
module Expr where

import GOperator

import qualified Data.ByteString as S
import Data.Word
import Data.Ratio

type Symbol = S.ByteString
data Cell t = Cell {unCell :: GCell} deriving (Eq, Ord, Show)

gRead :: Cell t -> Expr t
gRead = Read

data Expr t where
  BoolE :: Bool -> Expr Bool
  NumE :: (Real t) => t -> Expr t
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

-- Evaluating type-save Exprs to untyped GExprs suited for generation gcode
eval :: Expr t -> GExpr
eval (BoolE b) = G_Int . fromEnum $ b
eval (NumE n) = case toRational n of
  r | denominator r == 1 -> G_Int $ fromIntegral $ numerator r
    | otherwise -> G_Float $ realToFrac r
eval (Add e1 e2) = G_Add (eval e1) (eval e2)
eval (Sub e1 e2) = G_Sub (eval e1) (eval e2)
eval (And e1 e2) = G_And (eval e1) (eval e2)
eval (Not e1) = G_Not (eval e1)
eval (Eq e1 e2) = G_Eq (eval e1) (eval e2)
eval (Gt e1 e2) = G_Gt (eval e1) (eval e2)
eval (Read c) = G_Read $ unCell c
