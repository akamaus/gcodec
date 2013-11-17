module CNC.GTypes where

import CNC.Geometry(RealT)

data GInstr = GInstrI Char Int | GInstrF Char RealT deriving Show
data GLine = GFrame [GInstr] | GComment String deriving Show

data GProgram = GProgram {gpName :: String, gpCode :: [GLine]} deriving Show
