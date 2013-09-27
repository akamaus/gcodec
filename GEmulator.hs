module GEmulator where

import GCode
import GParser

import Control.Monad.Writer
import Control.Monad.Trans

import Data.Int
import Data.IORef
import Data.Maybe
import qualified Data.Map as M
import System.IO
import Text.Printf

data Pos = Pos {px :: RealT, py :: RealT, pz :: RealT} deriving (Show,Eq)
newtype Tool = Tool Int deriving (Show, Eq, Ord)
data Move = Move { m_p1 :: Pos, m_p2 :: Pos, m_feed::RealT, m_tool :: Maybe Tool } deriving Show

type GTrace = [Move]

fastSpeed = 20000 :: RealT

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
  tool <- newIORef (Just $ Tool 1)
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
        fast <- readIORef fast_move
        f <- case fast of
          False -> readIORef feed
          True -> return fastSpeed
        t <- case fast of
          False -> readIORef tool
          True -> return Nothing
        return $ Move p1 p2 f t
      read_pos = do
        [xx,yy,zz] <- mapM readIORef [x,y,z]
        return $ Pos xx yy zz
  mapM run_frame frames

data ProgramStatistics = ProgramStatistics {
  ps_time :: RealT,
  ps_tool_carve_stats :: M.Map Tool RealT,
  ps_move_dist :: RealT
  } deriving (Show)

iso7stats = iso7stats' 0 M.empty

iso7stats' time tool_map [] = ProgramStatistics { ps_time = time, ps_tool_carve_stats = tool_stats, ps_move_dist = move_dist }
  where tool_stats = M.mapKeys fromJust $ M.filterWithKey (\k _ -> isJust k) tool_map
        move_dist = fromMaybe 0 $ M.lookup Nothing tool_map
iso7stats' time tool_map (Move p1 p2 feed tool : moves) = iso7stats' (time + dt) tool_map' moves
  where tool_map' = M.insertWith (+) tool dt tool_map
        dst = dist p1 p2
        dt = if dst < eps then 0 else dist p1 p2 / feed

warn msg = hPutStrLn stderr msg

eps = 10**(-6) :: RealT
dist (Pos x1 y1 z1) (Pos x2 y2 z2) = sum $ map (^2) $ zipWith (-) [x1,y1,z1] [x2,y2,z2]

get_float (G_Float f) = f
get_float x = error $ printf "can't get contents of %s as Float" (show x)

get_int (G_Int i) = i
get_int x = error $ printf "can't get contents of %s as Int" (show x)
