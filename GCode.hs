{-# LANGUAGE OverloadedStrings #-}
module  GCode where

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
data GOperator = GOps [GOperator] | GLabel Label | GAssign GCell GExpr
               | GIf GExpr GOperator | GWhile Int GExpr GOperator | GGoto Label
               | GFrame [GInstruction ()] deriving Show

instance Monoid GOperator where
  mempty = GOps []
  mappend (GOps lst1) (GOps lst2) = GOps (lst1 ++ lst2)
  mappend (GOps lst) op = GOps (lst ++ [op])
  mappend op (GOps lst) = GOps (op:lst)
  mappend op1 op2 = GOps [op1, op2] -- error $ printf "can't sequence %s and %s" (show op1) (show op2)

type Label = S.ByteString
mkLabel :: String -> Label
mkLabel = S.pack . map (fromIntegral . fromEnum)

data GInstruction a = GInstrI Char Int -- integer compile type constant
                    | GInstrE Char GExpr deriving Show -- dynamic value

newtype GCell = GCell Word deriving (Eq, Ord, Show)


-- AST of concrete expression in ISO7
data GExpr = G_Add GExpr GExpr | G_Sub GExpr GExpr | G_Mul GExpr GExpr | G_Div GExpr GExpr
           | G_Gt GExpr GExpr | G_Eq GExpr GExpr | G_And GExpr GExpr | G_Or GExpr GExpr | G_Not GExpr
           | G_Int Int | G_Float Float | G_Read GCell deriving Show

type GopGen = Reader (Label -> Label)

gopGen :: GOperator -> GopGen Builder
gopGen (GOps ops) = do cs <- mapM gopGen ops
                       return $ mconcat cs
gopGen (GAssign cell expr) = return $ fromCell cell <> bs " = " <> gexprGen expr <> endl
gopGen (GGoto label) = do trans <- ask
                          return $ bs "GOTO " <> bs (trans label) <> endl
gopGen (GLabel label) = do trans <- ask
                           return $ bs (trans label) <> bs " "
gopGen (GIf cond branch) = do code <- gopGen branch
                              return $ bs "IF " <> gexprGen cond <> bs " THEN " <> code <> endl
gopGen (GWhile k cond body) = do code <- gopGen body
                                 return $ bs "WHILE " <> gexprGen cond <> bs " DO" <> fromShow k <> endl
                                   <> code
                                   <> bs "END" <> fromShow k <> endl
gopGen (GFrame codes) = return $ mconcat (intersperse (fromChar ' ') $ map ginstrGen codes) <> endl

gexprGen (G_Add e1 e2) = bracket $ gexprGen e1 <> bs " + " <> gexprGen e2
gexprGen (G_Sub e1 e2) = bracket $ gexprGen e1 <> bs " - " <> gexprGen e2
gexprGen (G_Mul e1 e2) = bracket $ gexprGen e1 <> bs " * " <> gexprGen e2
gexprGen (G_Div e1 e2) = bracket $ gexprGen e1 <> bs " / " <> gexprGen e2
gexprGen (G_Gt e1 e2) = bracket $ gexprGen e1 <> bs " GT " <> gexprGen e2
gexprGen (G_Eq e1 e2) = bracket $ gexprGen e1 <> bs " EQ " <> gexprGen e2
gexprGen (G_And e1 e2) = bracket $ gexprGen e1 <> bs " AND " <> gexprGen e2
gexprGen (G_Or e1 e2) =  bracket $ gexprGen e1 <> bs " OR "  <> gexprGen e2
gexprGen (G_Not e) =  bs "NOT "  <> gexprGen e
gexprGen (G_Read cell) = fromCell cell
gexprGen (G_Int i) = fromShow i
gexprGen (G_Float i) = fromShow i

ginstrGen (GInstrI c k) = fromChar c <> fromShow k
ginstrGen (GInstrE c e) = fromChar c <> gexprGen e

bracket s = bs "[ " <> s <> bs " ]"

fromCell (GCell n) = fromChar '#' <> fromShow n
bs = fromByteString
endl = fromChar '\n'

putGOps :: (Label -> Label) -> GOperator -> IO ()
putGOps label_trans g = LS.putStr $ toLazyByteString $ runReader (gopGen g) label_trans
