module GEmulator where

import GCode

import Data.Array.ST
import Data.STRef
import Control.Monad.ST
import Control.Monad.Writer.Strict
import Control.Monad.Trans

import Data.Int
import Data.IORef
import System.IO

data Pos = Pos {px :: RealT, py :: RealT, pz :: RealT} deriving (Show,Eq)

data Move = Move { m_p1 :: Pos, m_p2 :: Pos, m_feed::RealT} deriving Show
type GTrace = [Move]

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
      run_frame (GInstrE 'G' expr : rest) act = do case get_float expr of
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
get_float x = error $ "can't get contents of " ++ show x
