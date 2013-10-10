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
import Debug.Trace

-- Describes result of executing current frame
data FrameResult = FR_Stop | FR_Move Move

-- Describes move operation in declarative form
data Move = M_FastLinear { m_p1 :: Pos, m_p2 :: Pos }
          | M_Linear { m_p1 :: Pos, m_p2 :: Pos, m_feed::RealT, m_tool :: Tool }
          | M_CircleCenter { m_p1 :: Pos, m_p2 :: Pos, m_center :: Pos, m_dir :: CircleDirection,  m_feed::RealT, m_tool :: Tool }
          | M_CircleRadius { m_p1 :: Pos, m_p2 :: Pos, m_radius :: RealT, m_dir :: CircleDirection,  m_feed::RealT, m_tool :: Tool }
          | M_ToolChange { m_tool :: Tool }
            deriving Show

data CircleDirection = Clockwise | CounterClockwise deriving Show

-- Type of move, mapped from G codes, so no actuial arguments
data MoveType = MT_Fast | MT_Linear | MT_CW | MT_CCW
-- Miscellaneous operations
data OperationType = OT_NonOptionalStop | OT_ToolChange | OT_EndProgram deriving Show

newtype Tool = Tool Int deriving (Show, Eq, Ord)

type GTrace = [Move]

fastSpeed = 20000 :: RealT
toolChangeTime = 10 / 60 :: RealT -- in minutes
resetToLinear = True -- Should we switch to Linear interpolation after circle or not

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
  cur_frame_ref <- newIORef undefined -- for tracing purposes
  [x_ref,y_ref,z_ref] <- sequence $ replicate 3 $ newIORef 0
  [i_ref,j_ref,k_ref] <- sequence $ replicate 3 $ newIORef Nothing
  r_ref <- newIORef Nothing
  move_type_ref <- newIORef MT_Fast
  op_type_ref <- newIORef Nothing
  feed_ref <- newIORef 0
  tool_ref <- newIORef $ Tool 0
  tool_changer_ref <- newIORef $ Tool 0
  let -- processes entire frame, returns Move together with instructions what to do next
      run_frame (IFrame codes) = do moves <- proc_frame codes (return ())
                                    mop <- readIORef op_type_ref
                                    op_effs <- proc_operation
                                    move_effs <- read_effect moves
                                    return $ op_effs ++ move_effs
      -- Accumulates effects of a frame
      proc_frame [] act = return act
      proc_frame (InstrF axe val : rest) act | axe == 'X' = proc_move x_ref val rest act
                                             | axe == 'Y' = proc_move y_ref val rest act
                                             | axe == 'Z' = proc_move z_ref val rest act
                                             | axe == 'I' = proc_geom_setting i_ref val rest act
                                             | axe == 'J' = proc_geom_setting j_ref val rest act
                                             | axe == 'K' = proc_geom_setting k_ref val rest act
                                             | axe == 'R' = proc_geom_setting r_ref val rest act
      proc_frame (InstrF 'F' val : rest) act = writeIORef feed_ref val >> proc_frame rest act
      proc_frame (InstrI 'T' val : rest) act = writeIORef tool_changer_ref (Tool val) >> proc_frame rest act
      proc_frame (InstrI 'G' val : rest) act = proc_gcode val rest act
      proc_frame (InstrI 'M' val : rest) act = proc_mcode val rest act
      proc_frame (InstrI 'N' _ : rest) act = proc_frame rest act
      proc_frame (instr:rest) act = do warn $ "skipping instruction " ++ show instr
                                       proc_frame rest act
      -- Stores encountered G instruction
      proc_gcode val rest act = do
        case val of
          0 -> writeIORef move_type_ref MT_Fast
          1 -> writeIORef move_type_ref MT_Linear
          2 -> writeIORef move_type_ref MT_CW
          3 -> writeIORef move_type_ref MT_CCW
          n -> warn $ "Ignoring gcode " ++ show n
        proc_frame rest act
      -- Stores encountered G instruction
      proc_mcode val rest act = do
        m <- readIORef op_type_ref
        case m of
          Nothing -> do case val of
                          0 -> writeIORef op_type_ref $ Just OT_NonOptionalStop
                          6 -> writeIORef op_type_ref $ Just OT_ToolChange
                          30 -> writeIORef op_type_ref $ Just OT_EndProgram
                          n -> warn $ "Ignoring mcode " ++ show n
                        proc_frame rest act
          Just mcode -> fail $ "got second Mcode in single frame in addition to " ++ show mcode
      -- Stores instruction specifying target coordinates, like X Y Z
      proc_move :: IORef RealT -> RealT -> [Instr] -> IO () -> IO (IO ())
      proc_move axe val rest act = proc_frame rest $ do
        act
        writeIORef axe val
      -- Stores instruction specifying target coordinates, like X Y Z
      proc_geom_setting :: IORef (Maybe RealT) -> RealT -> [Instr] -> IO () -> IO (IO ())
      proc_geom_setting axe val rest act = do
        val0 <- readIORef axe
        case val0 of
          Nothing -> writeIORef axe $ Just val
          Just _ -> warn $ printf "can't set value %s, already specified code in that frame" (show val)
        proc_frame rest act

      proc_operation = do
        mop <- readIORef op_type_ref
        writeIORef op_type_ref Nothing
        case mop of
          Nothing -> return []
          Just OT_ToolChange -> do t <- readIORef tool_changer_ref
                                   writeIORef tool_ref t
                                   return [FR_Move $ M_ToolChange { m_tool = t }]
          Just OT_EndProgram -> return [FR_Stop]
          Just OT_NonOptionalStop -> return [FR_Stop]

      read_effect moves = do
        frame <- readIORef cur_frame_ref
        p1 <- read_pos
        moves
        p2 <- read_pos
        mt <- readIORef move_type_ref
        f <- readIORef feed_ref
        t <- readIORef tool_ref
        [r, i, j, k] <- mapM (\ref -> readIORef ref >>= \res -> writeIORef ref Nothing >> return res) [r_ref, i_ref,j_ref,k_ref]
        let radius_specified = isJust r
            center_specified = any isJust [i, j, k]
            circle_move dir | radius_specified = M_CircleRadius p1 p2 (fromJust r) dir f t
                            | center_specified = M_CircleCenter p1 p2 (Pos (read_pos i) (read_pos j) (read_pos k)) dir f t
                            | otherwise = error $ "circle interpolation was set, but neither center nor radius were specified in frame" ++ show frame
            read_pos = fromMaybe 0
        case mt of
          MT_CW -> writeIORef move_type_ref MT_Linear
          MT_CCW -> writeIORef move_type_ref MT_Linear
          _ -> return ()

        return $ (\x -> [x]) $ FR_Move $ case mt of
          MT_Fast -> M_FastLinear p1 p2
          MT_Linear -> M_Linear p1 p2 f t
          MT_CW -> circle_move Clockwise
          MT_CCW -> circle_move CounterClockwise
      read_pos = do
        [xx,yy,zz] <- mapM readIORef [x_ref,y_ref,z_ref]
        return $ Pos xx yy zz

      iterateFrames [] [] = return []
      iterateFrames (f:fs) [] = do
        writeIORef cur_frame_ref f
        res <- run_frame f
        iterateFrames fs res
      iterateFrames fs (r:rs) = case r of
        FR_Stop -> return []
        FR_Move m -> do next <- iterateFrames fs rs
                        return $ m : next
  iterateFrames frames []

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
  M_FastLinear p1 p2 -> let dst = dist p1 p2 in upd_stats dst Nothing tool_dists
  M_Linear p1 p2 _ _ -> let dst = dist p1 p2 in upd_work_stats dst
  M_CircleRadius p1 p2 r _ _ _ -> let dst = ark_dst p1 p2 r in trace (show (p1,p2, r, dst)) $  upd_work_stats dst
  M_CircleCenter p1 p2 c _ _ _ -> let r1 = dist p1 c
                                      r2 = dist p2 c
                                      dst = ark_dst p1 p2 r1
                                  in if abs (r1 - r2) < eps then upd_work_stats dst
                                     else  upd_work_stats 0--error $ "uneven distance between center and start and end points: " ++ show (c, p1,p2)
  M_ToolChange _ -> iso7stats' (time + toolChangeTime) fast_move_dist tool_dists moves
  where upd_work_stats dst = upd_stats dst (Just $ m_feed cur_mov) (upd_tools (m_tool cur_mov) dst)
        upd_stats dst mfeed tools' = case mfeed of
          Just feed -> iso7stats' (time + dt dst feed) fast_move_dist tools' moves
          Nothing -> iso7stats' (time + dt dst fastSpeed) (fast_move_dist + dst) tools' moves
        upd_tools tool dst = M.insertWith (+) tool dst tool_dists
        dt dst feed = if dst < eps then 0 else dst / feed

warn msg = hPutStrLn stderr msg

get_float (G_Real f) = f
get_float x = error $ printf "can't get contents of %s as Real" (show x)

get_int (G_Int i) = i
get_int x = error $ printf "can't get contents of %s as Int" (show x)
