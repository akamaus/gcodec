module CNC.IntegrationTests where

import CNC.Declarative
import CNC.HCode

import Data.Complex
import Control.Monad

flake_side = do
  [p1,p2,p3,p4,p5] <- declarePoints 5
  xSize p1 p2 1
  len p2 p3 1
  len p3 p4 1
  xAngle p2 p3 (pi / 3)
  xAngle p3 p4 (- pi / 3)
  xSize p4 p5 1

renderPolygon :: Int -> Path -> HCode ()
renderPolygon n path = render_side 0 path n
  where
    render_side p0 path 0 = return ()
    render_side p0 path k = do
      let d = deltaPath path
      renderPath (posToExpr p0) path
      render_side (p0 + d) (rotate (- 2*pi / fromIntegral n) path) (k-1)

test1 = do putStrLn "a snowflake"
           print $ (figure flake_side)
           putHCode $ renderPolygon 3 (figure flake_side)

integrationTests = do
  putStrLn "Integration tests"
  test1