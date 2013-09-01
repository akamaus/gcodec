{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
module Expr where

import GCode

import Prelude hiding((&&), (||), not,
                      (==), (>), (<))

import qualified AwePrelude as W

import qualified Data.ByteString as S
import Data.Word
import Data.Ratio

type Symbol = S.ByteString
data Cell t = Cell {unCell :: GCell} deriving (Eq, Ord, Show)

gRead :: Cell t -> Expr t
gRead = Read

class ToExpr t where
  toExpr :: t -> Expr t

{-
instance Integral t => ToExpr t where
  toExpr = IntE

instance RealFrac t => ToExpr t where
  toExpr = FloatE
-}

instance ToExpr Int where
  toExpr = IntE

instance ToExpr Double where
  toExpr = FloatE

data Expr t where
  BoolE :: Bool -> Expr W.Bool
  IntE :: Integral t => t -> Expr t
  FloatE :: RealFrac t => t -> Expr t
  Add :: Num t => Expr t -> Expr t -> Expr t
  Sub :: Num t => Expr t -> Expr t -> Expr t
  Mul :: Num t => Expr t -> Expr t -> Expr t
  Div :: Num t => Expr t -> Expr t -> Expr t
  And :: Expr W.Bool -> Expr W.Bool -> Expr W.Bool
  Or  :: Expr W.Bool -> Expr W.Bool -> Expr W.Bool
  Not :: Expr W.Bool -> Expr W.Bool
  Eq  :: Expr t -> Expr t -> Expr W.Bool
  Gt  :: Expr t -> Expr t -> Expr W.Bool
  Read :: Cell t -> Expr t

instance (Num t, ToExpr t) => Num (Expr t) where
  fromInteger = toExpr . fromInteger
  (+) = Add
  (-) = Sub
  (*) = Mul

instance (Fractional t, ToExpr t) => Fractional(Expr t) where
  fromRational = toExpr . fromRational
  (/) = Div

instance W.BoolC Expr where
  false = BoolE False
  true  = BoolE True
  (&&)  = And
  (||)  = And
  not   = Not

instance W.Eq Expr a where
  (==)  = Eq

instance W.Ord Expr a where
  (>) = Gt
  a < b = (W.>) b a

-- Evaluating type-save Exprs to untyped GExprs suited for generation gcode
eval :: Expr t -> GExpr
eval (BoolE b) = G_Int . fromEnum $ b
eval (IntE n) = G_Int $ fromIntegral n
eval (FloatE n) = G_Float $ realToFrac n

eval (Add e1 e2) = G_Add (eval e1) (eval e2)
eval (Sub e1 e2) = G_Sub (eval e1) (eval e2)
eval (Mul e1 e2) = G_Mul (eval e1) (eval e2)
eval (Div e1 e2) = G_Div (eval e1) (eval e2)

eval (Eq e1 e2) = G_Eq (eval e1) (eval e2)
eval (Gt e1 e2) = G_Gt (eval e1) (eval e2)
eval (And e1 e2) = G_And (eval e1) (eval e2)
eval (Or e1 e2) = G_Or (eval e1) (eval e2)
eval (Not e1) = G_Not (eval e1)

eval (Read c) = G_Read $ unCell c
