{-# LANGUAGE OverloadedStrings #-}
module  GOperator where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Monad.RWS
import Data.List
import Data.Maybe
import Data.Word
import qualified Data.ByteString as S

-- AST for program in ISO7
data GOperator = GOps [GOperator] | GLabel Label | GAssign GCell GExpr
               | GIf GExpr GOperator (Maybe GOperator) | GWhile GExpr GOperator | GGoto Label
               | GFrame [GInstruction]

type Label = S.ByteString

data GInstruction = G Int | M Int | X GExpr | Y GExpr | Z GExpr

newtype GCell = GCell Word


-- AST of concrete expression in ISO7
data GExpr = G_Add GExpr GExpr | G_Sub GExpr GExpr | G_GT GExpr GExpr | G_Eq GExpr GExpr | G_Int Int | G_Float Float | G_Read GCell

gopGen :: GOperator -> Builder
gopGen (GOps ops) = mconcat (map gopGen ops) <> endl
gopGen (GAssign cell expr) = fromCell cell <> bs " = " <> gexprGen expr <> endl
gopGen (GGoto label) = bs "GOTO " <> bs label
gopGen (GLabel label) = bs label <> bs ": "
gopGen (GIf cond branch1 mbranch2) = bs "IF " <> gexprGen cond <> bs " THEN " <> gopGen branch1 <> maybe mempty (\b2 -> bs " ELSE " <> gopGen b2) mbranch2 <> endl
gopGen (GFrame codes) = mconcat (intersperse (fromChar ' ') $ map gcodeGen codes) <> endl

gexprGen (G_Add e1 e2) = bracket $ gexprGen e1 <> bs " + " <> gexprGen e2
gexprGen (G_Sub e1 e2) = bracket $ gexprGen e1 <> bs " - " <> gexprGen e2
gexprGen (G_GT e1 e2) = bracket $ gexprGen e1 <> bs " GT " <> gexprGen e2
gexprGen (G_Eq e1 e2) = bracket $ gexprGen e1 <> bs " EQ " <> gexprGen e2
gexprGen (G_Read cell) = fromCell cell
gexprGen (G_Int i) = fromShow i

gcodeGen (G k) = fromChar 'G' <> fromShow k
gcodeGen (M k) = fromChar 'M' <> fromShow k
gcodeGen (X e) = fromChar 'X' <> gexprGen e
gcodeGen (Y e) = fromChar 'Y' <> gexprGen e
gcodeGen (Z e) = fromChar 'Z' <> gexprGen e

bracket s = bs "[ " <> s <> bs " ]"

fromCell (GCell n) = fromChar '#' <> fromShow n
bs = fromByteString
endl = fromChar '\n'