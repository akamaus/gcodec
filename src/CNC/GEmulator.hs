{-# LANGUAGE OverloadedStrings #-}
module Main where

import CNC.FanucMacro
import CNC.HCode
import CNC.GInterpreter
import CNC.GParser

import CNC.AwePrelude
--import Prelude(Num(..), Fractional(..), Floating(..), Int, ($), id, putStrLn, (++), Just)
import System.Environment

evaluateIsoFile :: FilePath -> IO ProgramStatistics
evaluateIsoFile file = do
  parsed <- parseIsoFile file
--  print parsed
  case parsed of
    Right iso -> do
      prog_trace <- gcodeToMoves iso
      return $ gcode_stats prog_trace
    Left err -> do putStrLn $ "Error parsing: " ++ show err
                   fail err

main = do
  [file] <- getArgs
  stats <- evaluateIsoFile file
  print stats
