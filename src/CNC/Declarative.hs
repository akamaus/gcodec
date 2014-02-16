{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module CNC.Declarative(module CNC.Figure,
                       mirrorX, mirrorY, rotate, scale, posToExpr, renderPath) where

import CNC.Figure
import CNC.Geometry(RealT, eps)
import CNC.HCode(HCode, frame, g, x, y,
                 Expr, toExpr)

import Data.Complex

-- mirrors around the X axis
mirrorX :: Path -> Path
mirrorX = mapPath (\(x :+ y) -> (x :+ negate y))

-- mirrors around the Y axis
mirrorY :: Path -> Path
mirrorY = mapPath (\(x :+ y) -> (negate x :+ y))

-- rotates around (0,0) point counter-clockwise on a given angle
rotate :: RealT -> Path -> Path
rotate a = mapPath (* exp (0 :+ a))

-- scales the path
scale :: RealT -> Path -> Path
scale k = mapPath (* (k :+ 0))

posToExpr :: Pos -> (Expr RealT, Expr RealT)
posToExpr (x :+ y) = (toExpr x, toExpr y)

-- renders a path in HCode monad
renderPath :: (Expr RealT, Expr RealT) -> Path -> HCode ()
renderPath (x0, y0) path = do
  frame [g 0, x x0, y y0]
  mapPathM_ (\(xi :+ yi) -> frame [g 1, x $ toExpr xi + x0, y $ toExpr yi + y0]) path
