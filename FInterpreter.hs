module FInterpreter where

import FanucMacro
import Geometry
import GTypes

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Array.IO
import Text.Printf

type FBank = IOArray Int FVal
data FVal = FV_Int Int | FV_Real RealT | FV_Undefined

instance Num FVal where
  (FV_Int v1) + (FV_Int v2) = FV_Int $ v1 + v2
  (FV_Real v1) + (FV_Real v2) = FV_Real $ v1 + v2

  (FV_Int v1) - (FV_Int v2) =FV_Int $ v1 - v2
  (FV_Real v1) - (FV_Real v2) = FV_Real $ v1 - v2

  (FV_Int v1) * (FV_Int v2) =FV_Int $ v1 * v2
  (FV_Real v1) * (FV_Real v2) = FV_Real $ v1 * v2

--(FV_Real v1) / (FV_Real v2) = FV_Real $ v1 / v2

type FInterpreter = ReaderT FBank (WriterT [GLine] IO)

runFIntereter :: FProgram -> IO [GLine]
runFIntereter fprog = do bank <- initBank
                         execWriterT $ runReaderT (interpretFCode fprog) bank

macroToGCode :: FProgram -> IO GProgram
macroToGCode fprog = do gcode <- runFIntereter fprog
                        return $ GProgram { gpName = "Interpreted from HCode", gpCode = gcode }

interpretFCode :: FOperator -> FInterpreter ()
interpretFCode (FOps ops comment) = do
  unless (null comment) $ tell $ [GComment comment]
  mapM_ interpretFCode ops

interpretFCode (FLabel lbl) = error "labels unimpl"

interpretFCode (FAssign cell expr) = do
  ind <- cellToInd cell
  val <- interpretFExpr expr
  set_bank ind val

interpretFCode (FFrame f_instrs) = do g_instrs <- mapM run_instr f_instrs
                                      tell $ [GFrame g_instrs]
  where run_instr (FInstrI c n) = return $ GInstrI c n
        run_instr (FInstrE c e) = do v <- interpretFExpr e
                                     return $ case v of
                                       FV_Int n -> GInstrI c n
                                       FV_Real n -> GInstrF c n

interpretFExpr :: FExpr -> FInterpreter FVal
interpretFExpr (F_Int n) = return $ FV_Int n
interpretFExpr (F_Real n) = return $ FV_Real n
interpretFExpr (F_Read cell) = do
  ind <- cellToInd cell
  get_bank ind
interpretFExpr (F_Add e1 e2) = do
  liftM2 (+) (interpretFExpr e1) (interpretFExpr e2)

cellToInd (FCell n) = return $ fromIntegral n
cellToInd t@(FTable table ind_expr) = error $ "unresolved table access" ++ show t

get_bank :: Int -> FInterpreter FVal
get_bank ind = do
  bank <- ask
  liftIO $ readArray bank ind

set_bank :: Int -> FVal -> FInterpreter ()
set_bank ind val = do
  bank <- ask
  liftIO $ writeArray bank ind val

get_float (F_Real f) = f
get_float x = error $ printf "can't get contents of %s as Real" (show x)

get_int (F_Int i) = i
get_int x = error $ printf "can't get contents of %s as Int" (show x)

initBank = newArray (1, 10000) (FV_Undefined)