module Main(main) where

import CNC.HCodeTests hiding(main)
import CNC.InterpreterTests
import CNC.DeclarativeTests

main = do
  generator_tests
  interpreter_tests
  declarative_tests
