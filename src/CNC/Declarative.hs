{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Declarative where

import CNC.Geometry(RealT)


import qualified Data.Map as M
import Data.Label
import qualified Data.Label.PureM as LM
import qualified Control.Monad.State as S
import Data.Complex

newtype PointInd = PointInd Int deriving (Eq, Ord, Enum, Show)
type Pos = Complex RealT

data Figure = Figure { _fPoints ::  M.Map PointInd (Maybe Pos), _fNextPoint :: PointInd, _fConstraints :: [Constraint] } deriving Show

mkPos x y = x :+ y

zero_ind = PointInd 0
empty_figure = Figure M.empty zero_ind []
figNumPoints fig = case _fNextPoint fig of
  PointInd k -> k

data Constraint = Disp PointInd PointInd Pos -- A vector from first point to second
                | Length PointInd PointInd RealT -- distance between points
                | Angle PointInd PointInd RealT deriving (Eq, Ord, Show)

mkLabels [''Figure]

-- Creates k new points
declare_points k = do
  next <- LM.gets fNextPoint
  let pts = take k $ iterate succ next
      pairs = map (\p -> (p, Nothing)) pts
  LM.modify fPoints (\ps -> ps `M.union` M.fromList pairs)
  LM.puts fNextPoint (succ $ last pts)
  return pts

xSize p1 p2 d = LM.modify fConstraints (Disp p1 p2 (mkPos d 0) :) -- distance between horizontally aligned points
ySize p1 p2 d = LM.modify fConstraints (Disp p1 p2 (mkPos 0 d) :) -- distance between vertically aligned points
len p1 p2 d = LM.modify fConstraints (Length p1 p2 d :) -- distance between points
xAngle p1 p2 a = LM.modify fConstraints (Angle p1 p2 a :) -- angle between line and axe X
yAngle p1 p2 a = LM.modify fConstraints (Angle p1 p2 (pi/2 - a) :) -- angle between line and axe Y

-- solves system consisting of N declared points and user specified constraints
solveFigure :: Figure -> [Pos]
solveFigure fig | figNumPoints fig == 0 = error "can't solve figure"
                | otherwise = let points = M.insert zeroInd (Just zeroPoint) $ get fPoints fig
                                  constraints = sort $ get fConstraints fig
                                  solution = solveConstraints points constraints
                              in checkSolution solution constraints

solveConstraints :: Map PointInd Pos -> [Constraint] -> [Constraint] -> Bool -> [Pos]
solveConstraints points [] [] _ = points -- no more constraints, finish here
solveConstraints points [] prev_cs True = solveConstraints points (reverse prev_cs) False -- iterating over constraints again
solveConstraints points [] prev_cs False = checkSolution points prev_cs -- no constraints was applied, finishing
solveConstraints points (c:cs) prev_cs shrinked = case c of -- process a single constraint
  Disp pi1 pi2 disp -> case (M.lookup pi1 points, M.lookup pi2 points) of
    (Nothing, Nothing) -> skipConstr
    (Just p1, Nothing) -> solveDisp p1 pi2 disp
    (Nothing, Just p2) -> solveDisp p2 pi1 (-disp)
    (Just p1, Just p2) -> checkConstr (abs $ p2 - p1 - disp)
  Length pi1 pi2 len -> case (M.lookup pi1 points, M.lookup pi2 points) of
    (Nothing, Nothing) -> skipConstr
    (Just p1, Nothing) -> solveLength p1 pi2 1 len
    (Nothing, Just p2) -> solveLength p2 pi1 (-1) len
    (Just p1, Just p2) -> checkConstr (abs (p1 - p2) - len)
  Angle pi1 pi2 a -> case (M.lookup pi1 points, M.lookup pi2 points) of -- only check these, they're used as part of Length constraints processing
    (Just p1, Just p2) -> checkConstr (phase (p2 - p1) - a)
    _ -> skipConstr
  where
    skipConstr = solveConstraints points cs (c:prev_cs) shrinked
    checkConstr dist = if dist < eps then solveConstraints points cs prev_cs shrinked else error $ "disagreement while processing " ++ show c
    solveDisp p ind d = solveConstraints (M.insert pi points, Just $ p + d) cs prev_cs True
    solveLength p ind ind0 d a_mult = case findAngleConstr ind0 ind of
      Nothing -> skipConstr
      Just angle) = solveConstraints (M.insert ind points, Just $ p + mkPolar len angle * a_mult) (deleteAngleConstr ind0 ind cs) prev_cs True

checkSolution points constrs | any isNothing $ M.elems points = error "couldn't find a complete solution, got " ++ show M.elems points
checkSolution points cs = let ps = M.map fromJust points
                          in checkConstrs ps cs

checkConstrs points = M.elems points
checkConstrs points (c:cs) = case c of
  Disp ind1 ind2 d =   check $ abs $ points ! ind2 - points ! ind1  - d
  Length ind1 ind2 l = check $ abs  (points ! ind2 - points ! ind1) - l
  Angle ind1 ind2 a  = check (phase (points ! ind2 - points ! ind1) - a)
 where check x = if abs x < eps then checkConstrs points cs else error $ printf "error checking constraint %s on points %d %d placed at %s %s"
                                                                                (show c) ind1 ind2 (show $ points ! ind1) (show $ points ! ind2)

-- finds first angle constraint on points with speified indices
findAngleConstr _ _ [] = Nothing
findAngleConstr ind1 ind2 (c:cs) = case c of
  Angle i1 i2 a | i1 == ind1 && i2 == ind2 -> Just a
                | i1 = ind2 && i2 == ind1 -> Just (-a)
  _ -> findAngleConstr ind1 ind2 cs

-- deletes first angle constraint
deleteAngleConstr ind1 ind2 = deleteBy (\_ (Angle i1 Angle i2) -> inds == (i1, i2) || inds == (i2, i1)) undefined
  where inds = (ind1,ind2)