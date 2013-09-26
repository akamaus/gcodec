{-# LANGUAGE OverloadedStrings #-}

import GCode
import HCode
import GEmulator
import GParser

import AwePrelude
--import Prelude(Num(..), Fractional(..), Floating(..), Int, ($), id, putStrLn, (++), Just)
import System.Environment

prog1 = do
  frame [g 0, x 0, y 0, z 0]
  x 10
  frame [g 1, f 10000]
  frame [g 1, x 10]
  frame [g 1, y 10]
  frame [g 1, z 10]

test = do
  Just (_, gcode) <- gcodeGen prog1
  putStrLn $ "***** gcode:"
  putHCode prog1
  let iso_code = macroToIso7 gcode
  putStrLn "*** iso code:"
  print iso_code
  gtrace  <- iso7ToMoves iso_code
  putStrLn "*** gtrace:"
  print gtrace
  print "****parsed iso7"
  evaluateIsoFile "examples/O192"

evaluateIsoFile :: FilePath -> IO ProgramStatistics
evaluateIsoFile file = do
  parsed <- parseIsoFile file
  case parsed of
    Right iso -> do
      prog_trace <- iso7ToMoves iso
      return $ iso7stats prog_trace
    Left err -> fail err

main = do
  [file] <- getArgs
  stats <- evaluateIsoFile file
  print stats