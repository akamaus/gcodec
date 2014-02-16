module CNC.DeclarativeTests where

import CNC.Declarative
import CNC.Geometry(eps)

import Data.Complex
import Control.Monad

fig1 = do [p1, p2] <- declarePoints 2
          xSize p1 p2 1

points1 = [0 :+ 0, 1 :+ 0]

fig2 = do [p1, p2, p3] <- declarePoints 3
          len p1 p2 1
          xAngle p1 p2 (pi/2)
          xAngle p2 p3 (-pi/2)
          len p2 p3 1

points2 = [0 :+ 0, 0 :+ 1.0, 0 :+ 0]

fig3 = do
  [p1,p2,p3,p4] <- declarePoints 4
  xSize p1 p2 1
  xAngle p2 p3 ( pi / 3)
  len p2 p3 2
  len p4 p3 2
  xAngle p3 p4 (-pi / 3)
--  xSize p3 p4 2

points3 = [0, 1, 1 + mkPolar 2 (pi/3), 3] --1 + mkPolar 2 (- 2*pi/3) + 2]

tests = [ (fig1,points1),
          (fig2, points2),
          (fig3, points3)
        ]

declarative_tests = do mapM_ run' tests
  where run' (fig, points) = do
          let solved = figure fig
          print solved
          when (any ((> 0.1) . magnitude) $ zipWith (-) (unPath solved) points) $ fail $ "test failed, should be " ++ show points
