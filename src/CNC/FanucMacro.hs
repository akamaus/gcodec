{-# LANGUAGE OverloadedStrings #-}
module  CNC.FanucMacro where

import CNC.Geometry(RealT)

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8(fromChar, fromShow)
import Control.Monad.RWS
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.String
import Data.Word
import Text.Printf
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as LS

type FProgram = FOperator
newtype Comment = Comment String deriving (Show,Eq)
instance IsString Comment where
  fromString = Comment

-- AST for program in ISO7
data FOperator = FOps [FOperator] Comment -- for aggregation of multiple operators into a program
               | FVoid -- fictive operator, mainly for comments
               | FLabel Label -- actially an O-code, but separated for better readability
               | FAssign FCell FExpr -- assignment macro operator
               | FIf FExpr Label | FWhile Int FExpr FOperator | FGoto Label -- if, while, goto operators
               | FFrame [FInstruction ()] deriving Show -- instructions of a single frame

type OpName = String

instance Monoid FOperator where
  mempty = FOps [] ""
  mappend (FOps lst1 "") (FOps lst2 "") = FOps (lst1 ++ lst2) ""
  mappend (FOps lst "") op = FOps (lst ++ [op]) ""
  mappend op (FOps lst "") = FOps (op:lst) ""
  mappend op1 op2 = FOps [op1, op2] ""

data Label = UserLabel String | AutoLabel Int deriving (Show, Eq, Ord)
mkULabel :: String -> Label
mkULabel = UserLabel

data FInstruction a = FInstrI Char Int -- integer compile type constant
                    | FInstrE Char FExpr deriving Show -- dynamic value

type TableName = S.ByteString
mkTableName :: String -> TableName
mkTableName = strToBS

data FCell = FCell Word -- regular numeric
           | FTable TableName FExpr -- symbolic table macro variable
           deriving (Eq, Ord, Show)

-- AST of concrete expression in ISO7
data FExpr = F_Unary OpName FExpr
           | F_Add FExpr FExpr | F_Sub FExpr FExpr | F_Mul FExpr FExpr | F_Div FExpr FExpr
           | F_Gt FExpr FExpr | F_Ge FExpr FExpr | F_Eq FExpr FExpr
           | F_And FExpr FExpr | F_Or FExpr FExpr | F_Xor FExpr FExpr
           | F_Int Int | F_Real RealT | F_Read FCell deriving (Eq, Ord, Show)

-- used for printing Labels on frames and in fotos
data LabelPrinter = LabelPrinter { lp_frame :: Label -> String, lp_ref :: Label -> String }
type FopGen = Reader LabelPrinter

fopGen :: FOperator -> Comment -> FopGen Builder
fopGen (FOps ops (Comment int_cmt_str)) (Comment ext_cmt_str) = do
  let cmt_str = if int_cmt_str == "" then ext_cmt_str else int_cmt_str
      comment = Comment cmt_str
  let size = length ops
  let comment_lines = case () of
        () | size <= 1 -> repeat comment
           | otherwise -> commentPrepend "START " comment : replicate (size-2) "" ++ [commentPrepend "END " comment]
  cs <- zipWithM fopGen ops comment_lines
  return $ mconcat cs

fopGen (FAssign cell expr) c = return $ fcellGen cell <> bs " = " <> fexprGen expr <> cmt c <> endl
fopGen (FGoto label) c = do trans <- asks lp_ref
                            return $ bs "GOTO " <> str (trans label) <> cmt c <> endl
fopGen (FLabel label) c = do trans <- asks lp_frame
                             return $ str (trans label) <> cmt c <> endl
fopGen (FIf cond branch_lbl) c = do goto_branch <- fopGen (FGoto branch_lbl) c
                                    return $ bs "IF " <> fexprGen cond <> bs " THEN " <> goto_branch
fopGen (FWhile k cond body) c = do code <- fopGen body ""
                                   return $ bs "WHILE " <> fexprGen cond <> bs " DO" <> fromShow k <> cmt c <> endl
                                     <> code
                                     <> bs "END" <> fromShow k <> endl
fopGen (FFrame codes) c = return $ mconcat (intersperse (fromChar ' ') $ map finstrGen codes) <> cmt c <> endl
fopGen FVoid c = return $ cmt c <> endl

fexprGen (F_Unary op e) = str op <> bs " " <> bracket (fexprGen e)
fexprGen (F_Add e1 e2) = bracket $ fexprGen e1 <> bs " + " <> fexprGen e2
fexprGen (F_Sub e1 e2) = bracket $ fexprGen e1 <> bs " - " <> fexprGen e2
fexprGen (F_Mul e1 e2) = bracket $ fexprGen e1 <> bs " * " <> fexprGen e2
fexprGen (F_Div e1 e2) = bracket $ fexprGen e1 <> bs " / " <> fexprGen e2
fexprGen (F_Gt e1 e2) = bracket $ fexprGen e1 <> bs " GT " <> fexprGen e2
fexprGen (F_Ge e1 e2) = bracket $ fexprGen e1 <> bs " GE " <> fexprGen e2
fexprGen (F_Eq e1 e2) = bracket $ fexprGen e1 <> bs " EQ " <> fexprGen e2
fexprGen (F_And e1 e2) = bracket $ fexprGen e1 <> bs " AND " <> fexprGen e2
fexprGen (F_Or e1 e2) =  bracket $ fexprGen e1 <> bs " OR "  <> fexprGen e2
fexprGen (F_Xor e1 e2) = error "buggy on cnc" -- bracket $ fexprGen e1 <> bs " XOR " <> fexprGen e2
fexprGen (F_Read cell) = fcellGen cell
fexprGen (F_Int i) = fromShow i
fexprGen (F_Real i) = fromShow i

finstrGen (FInstrI c k) = fromChar c <> fromShow k
finstrGen (FInstrE c e) = fromChar c <> fexprGen e

fcellGen (FCell n) = fromChar '#' <> fromShow n
fcellGen (FTable t e) = fromChar '#' <> bs t <> bracket (fexprGen e)

-- helpers
bracket s = bs "[ " <> s <> bs " ]"
commentPrepend str (Comment "") = ""
commentPrepend str (Comment cmt) = Comment $ str ++ cmt
-- conversion from basic types to builder
bs = fromByteString
str = bs . strToBS
endl = fromChar '\n'
cmt (Comment txt) = case txt of
  "" -> bs ""
  s -> bs " ( " <> str txt <> bs " )"

strToBS :: String -> S.ByteString
strToBS = S.pack . map (toEnum . fromEnum)



fmacroToBS :: LabelPrinter -> FOperator -> LS.ByteString
fmacroToBS printer f = toLazyByteString $ runReader (fopGen f "") printer

-- IO Printer
putFOps :: LabelPrinter -> FOperator -> IO ()
putFOps printer = LS.putStr . fmacroToBS printer
