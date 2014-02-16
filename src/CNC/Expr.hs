{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts, UndecidableInstances #-}
module CNC.Expr where

import CNC.FanucMacro
import CNC.Geometry(RealT)

import Prelude hiding((&&), (||), not,
                      (==), (>), (<))

import qualified CNC.AwePrelude as W

import qualified Data.ByteString as S
import Data.Word
import Data.Ratio

type Symbol = S.ByteString
data Cell t = Cell {unCell :: FCell} deriving (Eq, Ord, Show)

gRead :: Cell t -> Expr t
gRead = Read

class ToExpr t where
  toExpr :: t -> Expr t

{-
instance Integral t => ToExpr t where
  toExpr = IntE

instance RealFrac t => ToExpr t where
  toExpr = RealE
-}

instance ToExpr Int where
  toExpr = IntE

instance ToExpr RealT where
  toExpr = RealE

data Expr t where
  BoolE :: Bool -> Expr W.Bool
  IntE :: Integral t => t -> Expr t
  RealE :: RealFrac t => t -> Expr t
  Unary :: OpName -> Expr a -> Expr a
  Cast :: Expr a -> Expr t
  Add :: Num t => Expr t -> Expr t -> Expr t
  Sub :: Num t => Expr t -> Expr t -> Expr t
  Mul :: Num t => Expr t -> Expr t -> Expr t
  Div :: Num t => Expr t -> Expr t -> Expr t
  And :: Expr W.Bool -> Expr W.Bool -> Expr W.Bool
  Or  :: Expr W.Bool -> Expr W.Bool -> Expr W.Bool
  Not :: Expr W.Bool -> Expr W.Bool
  Eq  :: Expr t -> Expr t -> Expr W.Bool
  Gt  :: Expr t -> Expr t -> Expr W.Bool
  Ge  :: Expr t -> Expr t -> Expr W.Bool
  Read :: Cell t -> Expr t

instance (Num t, ToExpr t) => Num (Expr t) where
  fromInteger = toExpr . fromInteger
  (+) = Add
  (-) = Sub
  (*) = Mul

instance (Fractional t, ToExpr t) => Fractional(Expr t) where
  fromRational = toExpr . fromRational
  (/) = Div

{-
instance (Real (Expr t), Fractional (Expr t), ToExpr t) => RealFrac (Expr t) where
  round = Unary "ROUND" ::
-} -- FIXME, better define this, but how?

instance (Num t, ToExpr t, Floating t, Fractional (Expr t)) => Floating (Expr t) where
  pi = toExpr pi
  sin = Unary "SIN"
  cos = Unary "COS"
  tan = Unary "TAN"

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
  (>=) = Ge
  a < b = (W.>) b a
  a <= b = (W.>=) b a

-- Various gcode functions
fix :: (Fractional a, Integral b) => Expr a -> Expr b
fix = Cast . Unary "FIX"

fup :: (Fractional a, Integral b) => Expr a -> Expr b
fup = Cast . Unary "FUP"

fi :: (Integral a) => Expr a -> Expr RealT
fi = Cast

-- Evaluating type-save Exprs to untyped FExprs suited for generation gcode
eval :: Expr t -> FExpr
eval (BoolE b) = F_Int $ fromEnum b
eval (IntE n) =  F_Int $ fromIntegral n
eval (RealE n) = F_Real $ realToFrac n

eval (Unary op e) = F_Unary op (eval e)
eval (Cast e) = eval e

eval (Add e1 e2) = F_Add (eval e1) (eval e2)
eval (Sub e1 e2) = F_Sub (eval e1) (eval e2)
eval (Mul e1 e2) = F_Mul (eval e1) (eval e2)
eval (Div e1 e2) = F_Div (eval e1) (eval e2)

eval (Eq e1 e2) = F_Eq (eval e1) (eval e2)
eval (Gt e1 e2) = F_Gt (eval e1) (eval e2)
eval (Ge e1 e2) = F_Ge (eval e1) (eval e2)
eval (And e1 e2) = F_And (eval e1) (eval e2)
eval (Or e1 e2) = F_Or (eval e1) (eval e2)
eval (Not e1) = F_Sub (F_Int 1) (eval e1)

eval (Read c) = F_Read $ unCell c
