{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

--Taken from AwesomePrelude
--https://github.com/tomlokhorst/AwesomePrelude/blob/master/src/Generic/Data/Eq.hs

module CNC.AwePrelude.Eq where

import Prelude ()
import CNC.AwePrelude.Bool

infix 4 ==, /=

class Eq j a where
  (==) :: (BoolC j) => j a -> j a -> j Bool
  (/=) :: (BoolC j) => j a -> j a -> j Bool

  x /= y = not (x == y)
  x == y = not (x /= y)

instance (BoolC j) => Eq j Bool where
  x == y = if' x y (not y)
