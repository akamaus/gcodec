module FInterpreter where

import FanucMacro
import GParser

import Text.Printf

macroToGCode :: FOperator -> GProgram
macroToGCode op = GProgram {ipName = "UNKNOWN", ipCode = interpretMacro' op}
  where
    interpretMacro' :: FOperator -> [IFrame]
    interpretMacro' prog = case prog of
      FOps ops _ -> concatMap interpretMacro' ops
      FFrame codes -> [IFrame $ map interpretInstr codes]
      x -> error $ "can't interpter operator " ++ show x
    interpretInstr instr = case instr of
      FInstrE c (F_Real f) -> InstrF c f
      FInstrE c (F_Int i)   -> InstrI c i
      x -> error $ "can't interpret instruction " ++ show x

get_float (F_Real f) = f
get_float x = error $ printf "can't get contents of %s as Real" (show x)

get_int (F_Int i) = i
get_int x = error $ printf "can't get contents of %s as Int" (show x)
