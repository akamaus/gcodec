module GParser(Iso7Program(..), Instr(..), IFrame(..), parseIsoFile) where

import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Char
import Control.Applicative

data Instr = InstrI Char Int | InstrF Char Float deriving Show
newtype IFrame = IFrame [Instr] deriving Show

data Iso7Program = Iso7Program {ipName :: String, ipCode :: [IFrame]} deriving Show

instrP = do
  c <- toUpper <$> letter
  val <- double
  return $ case elem c "GMT" of
    True -> InstrI c (round val)
    _   -> InstrF c (realToFrac val)

frameP = IFrame <$> many1 instrP

frames = many $ do f <- frameP
                   skipComment
                   return f

iso7 = do
  char '%'
  skipSpace
  char 'O'
  prog <- many1 digit
  skipComment
  fs <- frames
  return $ Iso7Program {ipName = prog, ipCode = fs}

skipComment = do
  skipHorSpace
  res <- optional $ do
    char '('
    many $ notChar ')'
    char ')'
  skipSpace
  case res of
    Nothing -> return ()
    Just _ -> skipComment

skipHorSpace = many $ satisfy isHorizontalSpace

parseIsoFile file = do
  prog <- T.readFile file
  return $ parseOnly iso7 $ T.toStrict prog
