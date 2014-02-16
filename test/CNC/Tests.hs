module Main(main) where

import CNC.HCodeTests hiding(main)
import CNC.InterpreterTests
import CNC.DeclarativeTests
import CNC.IntegrationTests

main = do
  generator_tests
  interpreter_tests
  declarative_tests
  integrationTests