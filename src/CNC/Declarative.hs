{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module CNC.Declarative where

import CNC.Geometry(RealT, eps)


import qualified Data.Map as M
import Data.Map((!))
import Data.Maybe(fromJust, isNothing)
import Data.Label
import Data.List(deleteBy,sort)
import qualified Data.Label.PureM as LM
import qualified Control.Monad.State as S
import Data.Complex
import Text.Printf

newtype PointInd = PointInd Int deriving (Eq, Ord, Enum, Show)

type Pos = Complex RealT

type FigMap = M.Map PointInd (Maybe Pos)
data Figure = Figure { _fPoints ::  FigMap, _fNextPoint :: PointInd, _fConstraints :: [Constraint] } deriving Show

mkPos x y = x :+ y

zero_ind = PointInd 0
zero_point = mkPos 0 0

empty_figure = Figure M.empty zero_ind []
figNumPoints fig = case _fNextPoint fig of
  PointInd k -> k

instance (Ord a, RealFloat a) => Ord (Complex a) where
  compare x y = let c1 = compare (realPart x) (realPart y)
                in if c1 == EQ then compare (imagPart x) (imagPart y)
                   else c1

data UniConstraint pt = Disp {cPoint1 :: pt, cPoint2 :: pt, cPos :: Pos} -- A vector from first point to second
                      | Length {cPoint1 :: pt, cPoint2 :: pt, cLen :: RealT} -- distance between points
                      | Angle {cPoint1 :: pt, cPoint2 :: pt, cAngle :: RealT} deriving (Eq, Ord, Show)

instance Functor UniConstraint where
  fmap f c = case c of
    Disp p1 p2 d -> Disp (f p1) (f p2) d
    Length p1 p2 l -> Length (f p1) (f p2) l
    Angle p1 p2 a -> Angle (f p1) (f p2) a

type Constraint = UniConstraint PointInd

mkLabels [''Figure]

figure fig_m = let fig = S.execState fig_m empty_figure
               in solveFigure fig

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
                | otherwise = let points = M.insert zero_ind (Just zero_point) $ get fPoints fig
                                  constraints = sort $ get fConstraints fig
                                  solution = solveConstraints points constraints [] False
                              in map fromJust $ M.elems $ checkSolution solution constraints

solveConstraints :: FigMap -> [Constraint] -> [Constraint] -> Bool -> FigMap
solveConstraints points [] [] _ = points -- no more constraints, finish here
solveConstraints points [] prev_cs True = solveConstraints points (reverse prev_cs) [] False -- iterating over constraints again
solveConstraints points [] prev_cs False = points -- no constraints was applied, finishing
solveConstraints points (c:cs) prev_cs shrinked = case c of -- process a single constraint
  Disp pi1 pi2 disp -> case (points ! pi1, points ! pi2) of
    (Nothing, Nothing) -> skipConstr
    (Just p1, Nothing) -> solveDisp p1 pi2 disp
    (Nothing, Just p2) -> solveDisp p2 pi1 (-disp)
    (Just p1, Just p2) -> checkConstr
  Length pi1 pi2 len -> case (points ! pi1, points ! pi2) of
    (Nothing, Nothing) -> skipConstr
    (Just p1, Nothing) -> solveLength p1 pi1 pi2 1 len
    (Nothing, Just p2) -> solveLength p2 pi2 pi1 (-1) len
    (Just p1, Just p2) -> checkConstr
  Angle pi1 pi2 a -> case (M.lookup pi1 points, M.lookup pi2 points) of -- only check these, they're used as part of Length constraints processing
    (Just p1, Just p2) -> checkConstr
    _ -> skipConstr
  where
    pointConstr = fmap (fromJust . (points !)) c
    checkConstr = checkEps (calcDiscrepancy pointConstr) (solveConstraints points cs prev_cs shrinked) ("disagreement while processing " ++ show c)
    skipConstr = solveConstraints points cs (c:prev_cs) shrinked
    solveDisp p ind d = solveConstraints (M.insert ind (Just $ p + d) points) cs prev_cs True
    solveLength (p :: Pos) ind0 ind a_mult len = case findAngleConstr ind0 ind cs of
      Nothing -> skipConstr
      Just angle -> solveConstraints (M.insert ind (Just $ p + mkPolar len (angle * a_mult)) points)
                                     (deleteAngleConstr ind0 ind cs) prev_cs True

checkEps :: RealT -> a -> String -> a
checkEps val cont msg = if abs val < eps then cont else error $ msg

calcDiscrepancy :: (UniConstraint Pos) -> RealT
calcDiscrepancy c =
  case c of
    Disp p1 p2 d -> magnitude $ p2 - p1 - d
    Length p1 p2 l -> magnitude (p2 - p1) - l
    Angle p1 p2 a -> phase (p2 - p1) - a

checkSolution points constrs | any isNothing $ M.elems points = error $ "couldn't find a complete solution, got " ++ show (M.elems points)
checkSolution points cs = checkConstrs points cs

checkConstrs :: FigMap -> [UniConstraint PointInd] -> FigMap
checkConstrs points [] = points
checkConstrs points (c:cs) = check $ calcDiscrepancy $ fmap (fromJust . (points !)) c
 where p1 = cPoint1 c
       p2 = cPoint2 c
       check x = checkEps x (checkConstrs points cs) $
                   printf "error checking constraint %s on points %s %s placed at %s %s"
                   (show c) (show p1) (show p2) (show $ points ! p1) (show $ points ! p2)

-- finds first angle constraint on points with specified indices
findAngleConstr :: PointInd -> PointInd -> [Constraint] -> Maybe RealT
findAngleConstr _ _ [] = Nothing
findAngleConstr ind1 ind2 (c:cs) = case c of
  Angle i1 i2 a | i1 == ind1 && i2 == ind2 -> Just a
                | i1 == ind2 && i2 == ind1 -> Just (-a)
  _ -> findAngleConstr ind1 ind2 cs

-- deletes first angle constraint
deleteAngleConstr ind1 ind2 = deleteBy findAngle
                              (Angle ind1 ind2 undefined)
  where findAngle (Angle d1 d2 _) c2 =
          case c2 of
            (Angle i1 i2 _) -> (i1, i2) == (d2,d1) || (i2, i1) == (d1,d2)
            _ -> False