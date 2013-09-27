module Geometry where

type RealT = Float
data Pos = Pos {px :: RealT, py :: RealT, pz :: RealT} deriving (Show,Eq)
