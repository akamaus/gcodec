{-# LANGUAGE OverloadedStrings #-}
module GEmulatorTests where

import GCode
import HCode
import GEmulatorEngine
import GParser

import AwePrelude
--import Prelude(Num(..), Fractional(..), Floating(..), Int, ($), id, putStrLn, (++), Just)

prog1 = do
  frame [g 0, x 0, y 0, z 0]
  x 10000
  f 10000
  frame [g 1, y 10000]
  frame [g 1, z 10000]

gemulator_tests = do
  Just (_, gcode) <- gcodeGen prog1
  putStrLn $ "***** gcode:"
  putHCode prog1
  let iso_code = macroToIso7 gcode
  putStrLn "*** iso code:"
  print iso_code
  gtrace <- iso7ToMoves iso_code
  mapM_ print gtrace
  print $ iso7stats gtrace
  -- gtrace  <- iso7ToMoves iso_code
  -- print "****parsed iso7"
  -- evaluateIsoFile "examples/O192"
