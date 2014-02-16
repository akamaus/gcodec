module CNC.DeclarativeTests where

import CNC.Declarative
import CNC.Geometry(eps)

import Data.Complex
import Control.Monad

fig1 = do [p1, p2] <- declare_points 2
          xSize p1 p2 1

points1 = [0 :+ 0, 1 :+ 0]

fig2 = do [p1, p2, p3] <- declare_points 3
          len p1 p2 1
          xAngle p1 p2 (pi/2)
          xAngle p2 p3 (-pi/2)
          len p2 p3 1

points2 = [0 :+ 0, 0 :+ 1.0, 0 :+ 0]

tests = [ (fig1,points1),
          (fig2, points2)
        ]

declarative_tests = mapM_ run' tests
  where run' (fig, points) = do
          let solved = figure fig
          print solved
          when (any ((> 0.1) . magnitude) $ zipWith (-) solved points) $ fail $ "test failed, should be " ++ show points
