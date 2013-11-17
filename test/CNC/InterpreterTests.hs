{-# LANGUAGE OverloadedStrings #-}
module CNC.InterpreterTests where

import CNC.FanucMacro
import CNC.HCode
import CNC.FInterpreter
import CNC.GInterpreter
import CNC.GTypes

import CNC.AwePrelude
--import Prelude(Num(..), Fractional(..), Floating(..), Int, ($), id, putStrLn, (++), Just)

prog1 = do
  frame [g 0, x 0, y 0, z 0]
  x 10000
  f 10000
  frame [g 1, y 10000]
  frame [g 1, z 10000]

interpreter_tests = do
  Just (_, fcode) <- fanucGen prog1
  putStrLn $ "***** fcode:"
  putHCode prog1
  gcode <- fcodeToGCode fcode
  putStrLn "*** iso code:"
  print gcode
  gtrace <- gcodeToMoves gcode
  mapM_ print gtrace
  print $ gcode_stats gtrace
