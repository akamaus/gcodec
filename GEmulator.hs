module GEmulator where

import GCode
import GParser

import Control.Monad.Writer
import Control.Monad.Trans

import Data.Int
import Data.IORef
import System.IO
import Text.Printf

data Pos = Pos {px :: RealT, py :: RealT, pz :: RealT} deriving (Show,Eq)

data Move = Move { m_p1 :: Pos, m_p2 :: Pos, m_feed::RealT} deriving Show
type GTrace = [Move]

macroToIso7 :: GOperator -> Iso7Program
macroToIso7 op = Iso7Program {ipName = "UNKNOWN", ipCode = interpretMacro' op}
  where
    interpretMacro' :: GOperator -> [IFrame]
    interpretMacro' prog = case prog of
      GOps ops _ -> concatMap interpretMacro' ops
      GFrame codes -> [IFrame $ map interpretInstr codes]
      x -> error $ "can't interpter operator " ++ show x
    interpretInstr instr = case instr of
      GInstrE c (G_Float f) -> InstrF c f
      GInstrE c (G_Int i)   -> InstrI c i
      x -> error $ "can't interpret instruction " ++ show x

iso7ToMoves :: Iso7Program -> IO GTrace
iso7ToMoves (Iso7Program name frames) = do
  [x,y,z] <- sequence $ replicate 3 $ newIORef 0
  fast_move <- newIORef False
  feed <- newIORef 0
  instr <- newIORef 0
  let run_frame (IFrame codes) = do moves <- proc_frame codes (return ())
                                    read_effect moves
      proc_frame [] act = return act
      proc_frame (InstrF axe val : rest) act | axe == 'X' = proc_move x val rest act
                                             | axe == 'Y' = proc_move y val rest act
                                             | axe == 'Z' = proc_move z val rest act
      proc_frame (InstrF 'F' val : rest) act = writeIORef feed val >> proc_frame rest act
      proc_frame (InstrI 'G' val : rest) act = do case val of
                                                    0 -> writeIORef fast_move True
                                                    1 -> writeIORef fast_move False
                                                    n -> warn $ "Ignoring gcode " ++ show n
                                                  proc_frame rest act
      proc_frame (instr:rest) act = do warn $ "skipping instruction " ++ show instr
                                       proc_frame rest act
      proc_move axe val rest act = proc_frame rest $ (act >> writeIORef axe val)
      read_effect moves = do
        p1 <- read_pos
        moves
        p2 <- read_pos
        f <- readIORef feed
        return $ Move p1 p2 f
      read_pos = do
        [xx,yy,zz] <- mapM readIORef [x,y,z]
        return $ Pos xx yy zz
  mapM run_frame frames

warn msg = hPutStrLn stderr msg

get_float (G_Float f) = f
get_float x = error $ printf "can't get contents of %s as Float" (show x)

get_int (G_Int i) = i
get_int x = error $ printf "can't get contents of %s as Int" (show x)
