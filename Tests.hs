module Tests(main) where

import GeneratorTests hiding(main)
import InterpreterTests

main = do
  generator_tests
  interpreter_tests

