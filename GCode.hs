{-# LANGUAGE OverloadedStrings #-}
module  GCode where

import Geometry(RealT)

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8(fromChar, fromShow)
import Control.Monad.RWS
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.Word
import Text.Printf
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as LS

-- AST for program in ISO7
data GOperator = GOps [GOperator] Comment | GLabel Label | GAssign GCell GExpr
               | GIf GExpr Label | GWhile Int GExpr GOperator | GGoto Label
               | GFrame [GInstruction ()] deriving Show

type Comment = String
type OpName = String

instance Monoid GOperator where
  mempty = GOps [] []
  mappend (GOps lst1 c1) (GOps lst2 "") = GOps (lst1 ++ lst2) c1
  mappend g1@(GOps _ _) g2@(GOps _ _)= GOps [g1, g2] ""
  mappend (GOps lst c) op = GOps (lst ++ [op]) c
  mappend op (GOps lst "") = GOps (op:lst) ""
  mappend op g@(GOps _ _) = GOps [GOps [op] "", g] ""
  mappend op1 op2 = GOps [op1, op2] ""

data Label = UserLabel String | AutoLabel Int deriving (Show, Eq, Ord)
mkULabel :: String -> Label
mkULabel = UserLabel

data GInstruction a = GInstrI Char Int -- integer compile type constant
                    | GInstrE Char GExpr deriving Show -- dynamic value

type TableName = S.ByteString
mkTableName :: String -> TableName
mkTableName = strToBS

data GCell = GCell Word -- regular numeric
           | GTable TableName GExpr -- symbolic table macro variable
           deriving (Eq, Ord, Show)

-- AST of concrete expression in ISO7
data GExpr = G_Unary OpName GExpr
           | G_Add GExpr GExpr | G_Sub GExpr GExpr | G_Mul GExpr GExpr | G_Div GExpr GExpr
           | G_Gt GExpr GExpr | G_Ge GExpr GExpr | G_Eq GExpr GExpr
           | G_And GExpr GExpr | G_Or GExpr GExpr | G_Not GExpr
           | G_Int Int | G_Real RealT | G_Read GCell deriving (Eq, Ord, Show)

type GopGen = Reader (Label -> String)

gopGen :: GOperator -> GopGen Builder
gopGen (GOps ops comment) = do cs <- mapM gopGen ops
                               let cmt = case comment of
                                     "" -> bs ""
                                     s -> bs "( " <> str s <> bs " )" <> endl
                               return $ cmt <> mconcat cs
gopGen (GAssign cell expr) = return $ gcellGen cell <> bs " = " <> gexprGen expr <> endl
gopGen (GGoto label) = do trans <- ask
                          return $ bs "GOTO " <> str (trans label) <> endl
gopGen (GLabel label) = do trans <- ask
                           return $ str (trans label) <> bs " "
gopGen (GIf cond branch_lbl) = do goto_branch <- gopGen (GGoto branch_lbl)
                                  return $ bs "IF " <> gexprGen cond <> bs " THEN " <> goto_branch <> endl
gopGen (GWhile k cond body) = do code <- gopGen body
                                 return $ bs "WHILE " <> gexprGen cond <> bs " DO" <> fromShow k <> endl
                                   <> code
                                   <> bs "END" <> fromShow k <> endl
gopGen (GFrame codes) = return $ mconcat (intersperse (fromChar ' ') $ map ginstrGen codes) <> endl

gexprGen (G_Unary op e) = str op <> bs " " <> bracket (gexprGen e)
gexprGen (G_Add e1 e2) = bracket $ gexprGen e1 <> bs " + " <> gexprGen e2
gexprGen (G_Sub e1 e2) = bracket $ gexprGen e1 <> bs " - " <> gexprGen e2
gexprGen (G_Mul e1 e2) = bracket $ gexprGen e1 <> bs " * " <> gexprGen e2
gexprGen (G_Div e1 e2) = bracket $ gexprGen e1 <> bs " / " <> gexprGen e2
gexprGen (G_Gt e1 e2) = bracket $ gexprGen e1 <> bs " GT " <> gexprGen e2
gexprGen (G_Ge e1 e2) = bracket $ gexprGen e1 <> bs " K " <> gexprGen e2
gexprGen (G_Eq e1 e2) = bracket $ gexprGen e1 <> bs " EQ " <> gexprGen e2
gexprGen (G_And e1 e2) = bracket $ gexprGen e1 <> bs " AND " <> gexprGen e2
gexprGen (G_Or e1 e2) =  bracket $ gexprGen e1 <> bs " OR "  <> gexprGen e2
gexprGen (G_Not e) =  bs "NOT "  <> gexprGen e
gexprGen (G_Read cell) = gcellGen cell
gexprGen (G_Int i) = fromShow i
gexprGen (G_Real i) = fromShow i

ginstrGen (GInstrI c k) = fromChar c <> fromShow k
ginstrGen (GInstrE c e) = fromChar c <> gexprGen e

gcellGen (GCell n) = fromChar '#' <> fromShow n
gcellGen (GTable t e) = fromChar '#' <> bs t <> bracket (gexprGen e)

-- helpers
bracket s = bs "[ " <> s <> bs " ]"
-- conversion from basic types to builder
bs = fromByteString
str = bs . strToBS
endl = fromChar '\n'

strToBS :: String -> S.ByteString
strToBS = S.pack . map (toEnum . fromEnum)

gcodeToBS :: (Label -> String) -> GOperator -> LS.ByteString
gcodeToBS label_trans g = toLazyByteString $ runReader (gopGen g) label_trans

-- IO Printer
putGOps :: (Label -> String) -> GOperator -> IO ()
putGOps label_trans = LS.putStr . gcodeToBS label_trans
