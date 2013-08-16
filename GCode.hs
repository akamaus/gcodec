{-# LANGUAGE GADTs #-}
module GCode where

import Control.Monad.RWS


type GCode = RWS Int Int Int

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

gIf :: Expr Bool -> GCode () -> Gcode () -> GCode ()
gIf = undefined

(:=) :: Cell a -> Expr a -> GCode ()
(:=) = undefined

while :: Expr Bool -> GCode () -> GCode ()
while = undefined

goto :: Label -> GCode ()
goto = undefined



newCell :: String -> GCode Cell

instance Num t => Num (Expr t) where
  fromInteger = NumE . fromInteger
  (+) = Add
  (-) = Sub

  