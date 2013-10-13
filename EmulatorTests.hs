{-# LANGUAGE OverloadedStrings #-}

import GCode
import HCode
import GEmulator
import GParser

import AwePrelude
--import Prelude(Num(..), Fractional(..), Floating(..), Int, ($), id, putStrLn, (++), Just)
import System.Environment

prog1 = do
  frame [g 0, x 0, y 0, z 0]
  x 10000
  f 10000
  frame [g 1, y 10000]
  frame [g 1, z 10000]

test = do
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

evaluateIsoFile :: FilePath -> IO ProgramStatistics
evaluateIsoFile file = do
  parsed <- parseIsoFile file
--  print parsed
  case parsed of
    Right iso -> do
      prog_trace <- iso7ToMoves iso
      return $ iso7stats prog_trace
    Left err -> do putStrLn $ "Error parsing: " ++ show err
                   fail err

main = do
  [file] <- getArgs
  stats <- evaluateIsoFile file
  print stats