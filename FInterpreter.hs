module FInterpreter where

import FanucMacro
import Geometry
import GTypes

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Array.IO
import Text.Printf

type FBank = IOArray Int FVar
data FVar = V_Int Int | V_Real RealT

type FInterpreter = ReaderT FBank (WriterT [GFrame] IO)

interpretFCode (FOps ops) comment = do
  unless (null comment) $ tell $ GComment comment
  mapM_ interpretFCode ops

interpretFCode (FLabel lbl) = error "labels unimpl"

interpretFCode (FAssign cell expr) = do
  let ind = cellToInd cell
  val <- interpretFExpr expr
  set_bank ind val

macroToGCode :: FOperator -> GProgram
macroToGCode op = GProgram {ipName = "UNKNOWN", ipCode = interpretMacro' op}
  where
    interpretMacro' :: FOperator -> [GFrame]
    interpretMacro' prog = case prog of
      FOps ops _ -> concatMap interpretMacro' ops
      FFrame codes -> [GFrame $ map interpretInstr codes]
      x -> error $ "can't interpter operator " ++ show x
    interpretInstr instr = case instr of
      FInstrE c (F_Real f) -> GInstrF c f
      FInstrE c (F_Int i)   -> GInstrI c i
      x -> error $ "can't interpret instruction " ++ show x

get_float (F_Real f) = f
get_float x = error $ printf "can't get contents of %s as Real" (show x)

get_int (F_Int i) = i
get_int x = error $ printf "can't get contents of %s as Int" (show x)
