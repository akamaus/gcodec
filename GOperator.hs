{-# LANGUAGE OverloadedStrings #-}
module  GOperator where

import Control.Monad.RWS
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8

import Data.Word



data GOperator = GOps [GOperator] | GAssign GCell GExpr | GIf GExpr GOperator GOperator | GWhile GExpr GOperator | GCode GClass Int | GFrame [GOperator]

data GClass = G | M
newtype GCell = GCell Word

data GExpr = G_Add GExpr GExpr | G_Sub GExpr GExpr | G_GT GExpr GExpr | G_Eq GExpr GExpr | G_Int Int | G_Float Float | G_Read GCell

type GCode = RWS Int Int Int
--gIf :: Expr Bool -> GCode () -> Gcode () -> GCode ()
--gIf = undefined

--(:=) :: Cell a -> Expr a -> GCode ()
--(:=) = undefined

--while :: Expr Bool -> GCode () -> GCode ()
--while = undefined

--goto :: Label -> GCode ()
--goto = undefined

--newCell :: String -> GCode Cell



gopGen :: GOperator -> Builder
gopGen (GOps ops) = mconcat (map gopGen ops) <> endl
gopGen (GAssign cell expr) = fromCell cell <> bs " = " <> gexprGen expr <> endl

gexprGen (G_Add e1 e2) = braket $ gexprGen e1 <> bs " + " <> gexprGen e2
gexprGen (G_Sub e1 e2) = braket $ gexprGen e1 <> bs " - " <> gexprGen e2
gexprGen (G_GT e1 e2) = braket $ gexprGen e1 <> bs " GT " <> gexprGen e2
gexprGen (G_Eq e1 e2) = braket $ gexprGen e1 <> bs " EQ " <> gexprGen e2
gexprGen (G_Read cell) = fromCell cell
gexprGen (G_Int i) = fromShow i

braket s = bs "[ " <> s <> bs " ]"

fromCell (GCell n) = fromChar '#' <> fromShow n
bs = fromByteString
endl = fromChar '\n'