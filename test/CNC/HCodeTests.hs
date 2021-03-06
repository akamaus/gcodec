{-# LANGUAGE OverloadedStrings #-}
module CNC.HCodeTests where

import CNC.FanucMacro
import CNC.HCode

import CNC.AwePrelude
import Prelude(Num(..), Fractional(..), Floating(..), Int, ($), id, putStrLn, (++), show)
import Control.Monad(mapM_)

fcode_prog1 = FOps
  [ FLabel (UserLabel "start")
  , FAssign (FCell 101) (F_Add (F_Read $ FCell 101) (F_Int 42))
  , FFrame [FInstrI 'G' 1, FInstrE 'X' (F_Read $ FCell 101), FInstrE 'Y' (F_Read $ FCell 100), FInstrE 'Z' (F_Int 20)]
  , FAssign (FCell 100) (F_Sub (F_Read $ FCell 100) (F_Int 5))
  , FIf (F_Gt (F_Read (FCell 100)) (F_Int 0))
      (UserLabel "start")
  , FLabel (UserLabel "end")
  , FFrame [FInstrI 'M' 100]
  , FOps [FFrame [FInstrI 'M' 30]] ""
  ] "a test program"

hcode_prog1 :: HCode ()
hcode_prog1 = do
  label "start"
  var101 <- sysVar 5101
  var100 <- sysVar 5102
  var101 #= (var101 + 42)
  frame [g 1, z (var101), y (var100), z 20]
  var100 #= (var100 - 5)
  gIf (var100 > 0)
    (goto "start")
  m 30 # "stop operation"
  label "end"

hcode_prog2 :: HCode ()
hcode_prog2 = do
  let safe = 20
      step = 15
  frame [g 100, z safe]
  speed <- newVar 15 # "feed speed"
  speed2 <- newVar 2 # "rotation speed"
  cur_x <- newVar 5.0 # "current x"
  cur_y <- newVar 5.0
  count <- newVar (0 :: Int)
  while (cur_x < 100) $ do
    gwhile (cur_y < 200) $ do
      count #= count + 1
      frame [g 100, f (speed), s (speed2), x $ cur_x, y $ cur_y] # "fast move"
      frame [g 101, z 0]
      frame [g 100, z 20] # "drilling down"
      cur_x #= cur_x + step
      count #= count + fix cur_x # "just to test a round op"
    cur_y #= cur_y + step

hcode_poligon :: HCode ()
hcode_poligon = do
  cx <- newVar 100 # "center x"
  cy <- newVar 50 # "center y"
  rad <- newVar 20 # "radius"
  vertices <- newVar 6
  depth <- newVar 10 # "drill depth"
  instr <- newVar 1 # "instrument to use"
  lengths <- sysTable "_INSTR_LEN"

  angle <- newVar 0
  ver <- newVar 0
  step <- newVarE $ 360 / vertices
  while (ver < vertices + 1) $ do
    frame [g 0, x $ cx + rad * cos angle, y $ cy + rad * sin angle]
    frame [g 1, z $ depth + lengths instr]
    frame [g 0, z 0]
    lengths instr #= lengths instr - 0.01 # "compensate instrument wearing"
    angle #= angle + step

hcode_if_loops :: HCode ()
hcode_if_loops = do
  mx <- newVar 10 # "max x"
  my <- newVar 20 # "max y"
  comment "center"
  cx <- newVar 1 # "cur x"
  cy <- newVar 1 # "cur y"
  label "x-loop"
  gIf (cx <= mx) $ do
    x cx
    label "y-loop"
    gIf (cy <= my) $ do
      y cy
      cy #= cy + 1
      goto "y-loop"
    cx #= cx + 1
    goto "x-loop"
  m 30

hcode_for = do
  mx <- newVar 10
  for 1 (<= mx) (+ 0.5) $ \i -> do
    x i
    gIf (i == 5) $ break
    y 10
    y 0
  tst <- newVar (42 :: Int)
  m 30

hcode_gwhile_break = do
  k <- newVar (1 :: Int)
  comment "loop starts"
  gwhile (k < 100) $ do
    inner_loop k # "inner loop comment"
  m 30
 where inner_loop k = do
         x $ fi k
         gIf (k > 10) $ break
         y $ fi k

hcode_samples = [ (hcode_prog1, "Example1"),
            (hcode_prog2, "Example2"),
            (hcode_if_loops, "if_loops - test for compound operators in if branches"),
            (hcode_for, "for loop"),
            (hcode_poligon, "Poligon drawer"),
            (hcode_gwhile_break, "gwhile with break") ]

generator_tests = do
  putStrLn "***** FanucMacro example:"
  putFOps (LabelPrinter show show) fcode_prog1

  mapM_ gen_sample hcode_samples

 where gen_sample (hcode, descr) = do
         putStrLn $ "***** " ++ descr ++ ":"
         putHCode hcode
         putStrLn $ "***** " ++ descr ++ " Output finished\n"

main = generator_tests