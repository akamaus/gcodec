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

interpretMacro :: GOperator -> Iso7Program
interpretMacro op = Iso7Program {ipName = "UNKNOWN", ipCode = interpretMacro' op}
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

interpret :: GOperator -> IO GTrace
interpret prog = do
  [x,y,z] <- sequence $ replicate 3 $ newIORef 0
  fast_move <- newIORef False
  feed <- newIORef 0
  instr <- newIORef 0
  gtrace <- newIORef []
  let run (GOps ops _) = mapM_ run ops
      run (GFrame codes) = do moves <- run_frame codes (return ())
                              eff <- read_effect moves
                              modifyIORef' gtrace (eff:)
      run x = fail $ "interpreter is incompete, can't run " ++ show x
      run_frame [] act = return act
      run_frame (GInstrE axe expr : rest) act | axe == 'X' = run_move x expr rest act
                                              | axe == 'Y' = run_move y expr rest act
                                              | axe == 'Z' = run_move z expr rest act
      run_frame (GInstrE 'F' expr : rest) act = writeIORef feed (get_float expr) >> run_frame rest act
      run_frame (GInstrE 'G' expr : rest) act = do case get_int expr of
                                                     0 -> writeIORef fast_move True
                                                     1 -> writeIORef fast_move False
                                                     n -> warn $ "Ignoring gcode " ++ show n
                                                   run_frame rest act
      run_frame (GInstrE axe expr : rest) act = do warn $ "skipping axe" ++ show axe
                                                   run_frame rest act
      run_frame (instr:rest) act = do warn $ "skipping instruction " ++ show instr
                                      run_frame rest act
      run_move axe expr rest act = run_frame rest $ (act >> writeIORef axe (get_float expr))
      read_effect moves = do
        p1 <- read_pos
        moves
        p2 <- read_pos
        f <- readIORef feed
        return $ Move p1 p2 f
      read_pos = do
        [xx,yy,zz] <- mapM readIORef [x,y,z]
        return $ Pos xx yy zz
  run prog
  trace <- readIORef gtrace
  return $ reverse trace


warn msg = hPutStrLn stderr msg

get_float (G_Float f) = f
get_float x = error $ printf "can't get contents of %s as Float" (show x)

get_int (G_Int i) = i
get_int x = error $ printf "can't get contents of %s as Int" (show x)
