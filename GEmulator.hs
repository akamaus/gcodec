{-# LANGUAGE OverloadedStrings #-}
module Main where

import FanucMacro
import HCode
import GInterpreter
import GParser

import AwePrelude
--import Prelude(Num(..), Fractional(..), Floating(..), Int, ($), id, putStrLn, (++), Just)
import System.Environment

evaluateIsoFile :: FilePath -> IO ProgramStatistics
evaluateIsoFile file = do
  parsed <- parseIsoFile file
--  print parsed
  case parsed of
    Right iso -> do
      prog_trace <- iso7ToMoves iso
      return $ iso7stats prog_trace
    Left err -> do putStrLn $ "Error parsing: " ++ show err
                   fail err

main = do
  [file] <- getArgs
  stats <- evaluateIsoFile file
  print stats
