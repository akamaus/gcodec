module  GCode where

import Control.Monad.RWS
import Blaze.ByteString.Builder
import Data.Word

type GCode = RWS Int Int Int

data GOperator = GOps [GOperator] | GAssign GCell GExpr | GIf GExpr GOperator GOperator | GWhile GExpr GOperator | GCode GClass Int | GFrame [GOperator]

data GClass = G | M
newtype GCell = GCell Word

data GExpr = G_Add GExpr GExpr | G_Sub GExpr GExpr | G_GT GExpr GExpr | G_Eq GExpr GExpr | G_Int Int | G_Float Float | G_Read GCell
{-
data GOperator where
  Code CodeType 
-}


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
gopGen (GOps ops) = mconcat (gopGen ops) <> endl
gopGen (GAssign (GCell n) expr) = wordDec n <> bs " = " <> gexprGen expr <> endl

gexprGen (G_Add e1 e2) = braket $ gexprGen e1 <> bs " + " <> genExpr e2
gexprGen (G_Sub e1 e2) = braket $ gexprGen e1 <> bs " - " <> genExpr e2
gexprGen (G_GT e1 e2) = braket $ gexprGen e1 <> bs " GT " <> genExpr e2
gexprGen (G_Eq e1 e2) = braket $ gexprGen e1 <> bs " EQ " <> genExpr e2

braket s = bs "[ " <> s <> " ]"

bs = bytestring
endl = char8 '\n'