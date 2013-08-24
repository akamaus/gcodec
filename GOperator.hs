{-# LANGUAGE OverloadedStrings #-}
module  GOperator where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8(fromChar, fromShow)
import Control.Monad.RWS
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.Word
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as LS

-- AST for program in ISO7
data GOperator = GOps [GOperator] | GLabel Label | GAssign GCell GExpr
               | GIf GExpr GOperator | GWhile GExpr GOperator | GGoto Label
               | GFrame [GInstruction] deriving Show

instance Monoid GOperator where
  mempty = GOps []
  mappend (GOps lst1) (GOps lst2) = GOps (lst1 ++ lst2)
  mappend (GOps lst) op = GOps (lst ++ [op])
  mappend op (GOps lst) = GOps (op:lst)

type Label = S.ByteString
mkLabel :: String -> Label
mkLabel = S.pack . map (fromIntegral . fromEnum)

data GInstruction = G Int | M Int | X GExpr | Y GExpr | Z GExpr deriving Show

newtype GCell = GCell Word deriving (Eq, Ord, Show)


-- AST of concrete expression in ISO7
data GExpr = G_Add GExpr GExpr | G_Sub GExpr GExpr
           | G_Gt GExpr GExpr | G_Eq GExpr GExpr | G_And GExpr GExpr | G_Not GExpr
           | G_Int Int | G_Float Float | G_Read GCell deriving Show

type GopGen = Reader (Label -> Label)

gopGen :: GOperator -> GopGen Builder
gopGen (GOps ops) = do cs <- mapM gopGen ops
                       return $ mconcat cs <> endl
gopGen (GAssign cell expr) = return $ fromCell cell <> bs " = " <> gexprGen expr <> endl
gopGen (GGoto label) = do trans <- ask
                          return $ bs "GOTO " <> bs (trans label)
gopGen (GLabel label) = do trans <- ask
                           return $ bs (trans label) <> bs ": "
gopGen (GIf cond branch) = do code <- gopGen branch
                              return $ bs "IF " <> gexprGen cond <> bs " THEN " <> code <> endl
gopGen (GFrame codes) = return $ mconcat (intersperse (fromChar ' ') $ map ginstrGen codes) <> endl

gexprGen (G_Add e1 e2) = bracket $ gexprGen e1 <> bs " + " <> gexprGen e2
gexprGen (G_Sub e1 e2) = bracket $ gexprGen e1 <> bs " - " <> gexprGen e2
gexprGen (G_Gt e1 e2) = bracket $ gexprGen e1 <> bs " GT " <> gexprGen e2
gexprGen (G_Eq e1 e2) = bracket $ gexprGen e1 <> bs " EQ " <> gexprGen e2
gexprGen (G_Read cell) = fromCell cell
gexprGen (G_Int i) = fromShow i

ginstrGen (G k) = fromChar 'G' <> fromShow k
ginstrGen (M k) = fromChar 'M' <> fromShow k
ginstrGen (X e) = fromChar 'X' <> gexprGen e
ginstrGen (Y e) = fromChar 'Y' <> gexprGen e
ginstrGen (Z e) = fromChar 'Z' <> gexprGen e

bracket s = bs "[ " <> s <> bs " ]"

fromCell (GCell n) = fromChar '#' <> fromShow n
bs = fromByteString
endl = fromChar '\n'

putGOps :: (Label -> Label) -> GOperator -> IO ()
putGOps label_trans g = LS.putStr $ toLazyByteString $ runReader (gopGen g) label_trans
