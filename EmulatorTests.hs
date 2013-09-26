{-# LANGUAGE OverloadedStrings #-}

import GCode
import HCode
import GEmulator

import AwePrelude
--import Prelude(Num(..), Fractional(..), Floating(..), Int, ($), id, putStrLn, (++), Just)

prog1 = do
  frame [g 0, x 0, y 0, z 0]
  frame [g 1, f 10000]
  frame [g 1, x 10]
  frame [g 1, y 10]
  frame [g 1, z 10]

main = do
  Just (_, gcode) <- gcodeGen prog1
  putStrLn $ "***** gcode:"
  putHCode prog1
  let iso_code = interpretMacro gcode
  putStrLn "*** iso code:"
  print iso_code
  gtrace  <- interpret gcode
  putStrLn "*** gtrace:"
  print gtrace
  print "****parsed gcode"
--  let bs = gcodeToBS
