module Tests(main) where

import HCodeTests hiding(main)
import InterpreterTests

main = do
  generator_tests
  interpreter_tests

