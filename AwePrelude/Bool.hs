{-# LANGUAGE EmptyDataDecls #-}

--Taken from AwesomePrelude
--https://github.com/tomlokhorst/AwesomePrelude/blob/master/src/Generic/Data/Bool.hs

module AwePrelude.Bool where

import Prelude ()

infixr 3 &&
infixr 2 ||

data Bool
class BoolC j where
  false :: j Bool
  true :: j Bool
  bool :: j a -> j a -> j Bool -> j a
  (||) :: j Bool -> j Bool -> j Bool
  (&&) :: j Bool -> j Bool -> j Bool
  not :: j Bool -> j Bool
  x || y = bool y true x
  x && y = bool false y x
  not = bool true false

if' :: BoolC j => j Bool -> j a -> j a -> j a
if' b x y = bool y x b





