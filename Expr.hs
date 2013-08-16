{-# LANGUAGE GADTs #-}
module Types where


data Cell t = CellSym String

data Expr t where
  BoolE :: Bool -> Expr Bool
  NumE :: Num t => t -> Expr t
  Add :: Num t => Expr t -> Expr t -> Expr t
  Sub :: Num t => Expr t -> Expr t -> Expr t
  And :: Expr Bool -> Expr Bool -> Expr Bool
  Not :: Expr Bool -> Expr Bool
  Read :: Cell t -> Expr t

deriving Show


instance Num t => Num (Expr t) where
  fromInteger = NumE . fromInteger
  (+) = Add
  (-) = Sub

  