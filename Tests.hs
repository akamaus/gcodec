module Tests(main) where

import GeneratorTests hiding(main)
import GEmulatorTests

main = do
  generator_tests
  gemulator_tests

