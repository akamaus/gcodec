module CNC.Geometry where

type RealT = Float
data Pos = Pos {px :: RealT, py :: RealT, pz :: RealT} deriving (Show,Eq)

eps = 10**(-6) :: RealT
dist (Pos x1 y1 z1) (Pos x2 y2 z2) = sqrt $ sum $ map (^2) $ zipWith (-) [x1,y1,z1] [x2,y2,z2]

ark_dst p1 p2 r = let a = dist p1 p2
                      a' = if a > 2*r && abs (a / (2*r) - 1) < eps then 2*r - eps else a -- FIXME correction for minor circle parameters inconsistences
                      alpha = 2 * asin (a' / 2 / r)
                  in alpha * r


