module Main(main) where

import CNC.HCodeTests hiding(main)
import CNC.InterpreterTests

main = do
  generator_tests
  interpreter_tests

