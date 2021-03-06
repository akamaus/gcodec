module CNC.GParser(module CNC.GTypes, parseIsoFile) where

import CNC.GTypes

import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Char
import Control.Applicative
import Data.Maybe

instrP = do
  c <- toUpper <$> letter
  val <- iso_double
  return $ case elem c "GMNT" of
    True -> GInstrI c (round val)
    _   -> GInstrF c (realToFrac val)

frameP = GFrame <$> many1 instrP

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
  char '%'
  skipComment
  endOfInput
  return $ GProgram {gpName = prog, gpCode = fs}

skipComment = do
  skipHorSpace
  optional $ char ';'
  res <- optional $ do
    char '('
    many $ notChar ')'
    char ')'
    skipHorSpace
  res2 <- optional $ satisfy isEndOfLine
  case isJust res || isJust res2 of
    False -> return ()
    True -> skipComment

skipHorSpace = many $ satisfy isHorizontalSpace

iso_double = do minus <- optional $ char '-'
                case minus of
                  Nothing -> iso_pos_double
                  Just _ -> negate <$> iso_pos_double

iso_pos_double = leading_dot <|> (double >>= \d ->  optional (char '.') >> return d)
leading_dot = do char '.'
                 n <- number
                 case n of
                   I i -> let len = length (show i)
                          in return $ fromIntegral i / (10^len)
                   _ -> fail "strange number with leading dot"

parseIsoFile file = do
  prog <- T.readFile file
  return $ parseOnly iso7 $ T.toStrict prog
