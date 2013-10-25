{-# LANGUAGE OverloadedStrings #-}
module InterpreterTests where

import FanucMacro
import HCode
import FInterpreter
import GInterpreter
import GTypes

import AwePrelude
--import Prelude(Num(..), Fractional(..), Floating(..), Int, ($), id, putStrLn, (++), Just)

prog1 = do
  frame [g 0, x 0, y 0, z 0]
  x 10000
  f 10000
  frame [g 1, y 10000]
  frame [g 1, z 10000]

interpreter_tests = do
  Just (_, gcode) <- fanucGen prog1
  putStrLn $ "***** gcode:"
  putHCode prog1
  let gcode_code = macroToGCode gcode
  putStrLn "*** iso code:"
  print gcode_code
  gtrace <- gcodeToMoves gcode_code
  mapM_ print gtrace
  print $ gcode_stats gtrace
  -- gtrace  <- iso7ToMoves iso_code
  -- print "****parsed iso7"
  -- evaluateIsoFile "examples/O192"
