module GEmulator where

import GCode
import Geometry
import GParser

import Control.Monad.Writer
import Control.Monad.Trans

import Data.Int
import Data.IORef
import Data.Maybe
import qualified Data.Map as M
import System.IO
import Text.Printf

newtype Tool = Tool Int deriving (Show, Eq, Ord)
data Move = M_FastLinear { m_p1 :: Pos, m_p2 :: Pos }
          | M_Linear { m_p1 :: Pos, m_p2 :: Pos, m_feed::RealT, m_tool :: Tool }
          | M_CircleCenter { m_p1 :: Pos, m_p2 :: Pos, m_center :: Pos, m_dir :: CircleDirection,  m_feed::RealT, m_tool :: Tool }
          | M_CircleRadius { m_p1 :: Pos, m_p2 :: Pos, m_radius :: RealT, m_dir :: CircleDirection,  m_feed::RealT, m_tool :: Tool }
            deriving Show

data CircleDirection = Clockwise | CounterClockwise deriving Show

data MoveType = MT_Fast | MT_Linear | MT_CW | MT_CCW

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
      GInstrE c (G_Real f) -> InstrF c f
      GInstrE c (G_Int i)   -> InstrI c i
      x -> error $ "can't interpret instruction " ++ show x

iso7ToMoves :: Iso7Program -> IO GTrace
iso7ToMoves (Iso7Program name frames) = do
  [x_ref,y_ref,z_ref] <- sequence $ replicate 3 $ newIORef 0
  [i_ref,j_ref,k_ref] <- sequence $ replicate 3 $ newIORef Nothing
  r_ref <- newIORef Nothing
  move_type_ref <- newIORef MT_Fast
  feed_ref <- newIORef 0
  tool_ref <- newIORef $ Tool 1
  let run_frame (IFrame codes) = do moves <- proc_frame codes (return ())
                                    read_effect moves
      proc_frame [] act = return act
      proc_frame (InstrF axe val : rest) act | axe == 'X' = proc_move x_ref val rest act
                                             | axe == 'Y' = proc_move y_ref val rest act
                                             | axe == 'Z' = proc_move z_ref val rest act
                                             | axe == 'I' = proc_geom_setting i_ref val rest act
                                             | axe == 'J' = proc_geom_setting j_ref val rest act
                                             | axe == 'K' = proc_geom_setting k_ref val rest act
                                             | axe == 'R' = proc_geom_setting r_ref val rest act
      proc_frame (InstrF 'F' val : rest) act = writeIORef feed_ref val >> proc_frame rest act
      proc_frame (InstrI 'G' val : rest) act = do case val of
                                                    0 -> writeIORef move_type_ref MT_Fast
                                                    1 -> writeIORef move_type_ref MT_Linear
                                                    2 -> writeIORef move_type_ref MT_CW
                                                    3 -> writeIORef move_type_ref MT_CCW
                                                    n -> warn $ "Ignoring gcode " ++ show n
                                                  proc_frame rest act
      proc_frame (instr:rest) act = do warn $ "skipping instruction " ++ show instr
                                       proc_frame rest act
      proc_move :: IORef RealT -> RealT -> [Instr] -> IO () -> IO (IO ())
      proc_move axe val rest act = proc_frame rest $ do
        act
        writeIORef axe val
      proc_geom_setting :: IORef (Maybe RealT) -> RealT -> [Instr] -> IO () -> IO (IO ())
      proc_geom_setting axe val rest act = do
        val0 <- readIORef axe
        case val0 of
          Nothing -> writeIORef axe $ Just val
          Just _ -> warn $ printf "can't set value %s, already specified code in that frame" (show val)
        proc_frame rest act

      read_effect moves = do
        p1 <- read_pos
        moves
        p2 <- read_pos
        mt <- readIORef move_type_ref
        f <- readIORef feed_ref
        t <- readIORef tool_ref
        [r, i, j, k] <- mapM readIORef [r_ref, i_ref,j_ref,k_ref]
        let radius_specified = isJust r
            center_specified = any isJust [i, j, k]
            circle_move dir | radius_specified = M_CircleRadius p1 p2 (fromJust r) dir f t
                            | center_specified = M_CircleCenter p1 p2 (Pos (read_pos i) (read_pos j) (read_pos k)) dir f t
                            | otherwise = error "circle interpolation was set, but neither center nor radius weren't specified"
            read_pos = fromMaybe 0
        return $ case mt of
          MT_Fast -> M_FastLinear p1 p2
          MT_Linear -> M_Linear p1 p2 f t
          MT_CW -> circle_move Clockwise
          MT_CCW -> circle_move CounterClockwise
      read_pos = do
        [xx,yy,zz] <- mapM readIORef [x_ref,y_ref,z_ref]
        return $ Pos xx yy zz
  mapM run_frame frames

data ProgramStatistics = ProgramStatistics {
  ps_time :: RealT,
  ps_fast_move_dist :: RealT,
  ps_tool_carve_stats :: M.Map Tool RealT
  } deriving (Show)

iso7stats = iso7stats' 0 0 M.empty

iso7stats' time fast_move_dist tool_dists [] = ProgramStatistics { ps_time = time
                                                               , ps_fast_move_dist = fast_move_dist
                                                               , ps_tool_carve_stats = tool_dists
                                                               }
iso7stats' time fast_move_dist tool_dists (cur_mov : moves) = case cur_mov of
  M_FastLinear p1 p2 -> let dst = dist p1 p2 in iso7stats' (time + dt dst fastSpeed) (fast_move_dist + dst) tool_dists moves
  M_Linear p1 p2 feed tool -> let dst = dist p1 p2 in iso7stats' (time + dt dst feed) fast_move_dist (upd_tools tool dst) moves
  _ -> iso7stats' time fast_move_dist  tool_dists moves
  where upd_tools tool dst = M.insertWith (+) tool dst tool_dists
        dt dst feed = if dst < eps then 0 else dst / feed

warn msg = hPutStrLn stderr msg

eps = 10**(-6) :: RealT
dist (Pos x1 y1 z1) (Pos x2 y2 z2) = sqrt $ sum $ map (^2) $ zipWith (-) [x1,y1,z1] [x2,y2,z2]

get_float (G_Real f) = f
get_float x = error $ printf "can't get contents of %s as Real" (show x)

get_int (G_Int i) = i
get_int x = error $ printf "can't get contents of %s as Int" (show x)
