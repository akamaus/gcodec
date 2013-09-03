{-# LANGUAGE OverloadedStrings #-}

import GCode
import HCode

import AwePrelude
import Prelude(Num(..), Int, ($), id, putStrLn)

gcode_prog1 = GOps
  [ GLabel "start"
  , GAssign (GCell 101) (G_Add (G_Read $ GCell 101) (G_Int 42))
  , GFrame [GInstrI 'G' 1, GInstrE 'X' (G_Read $ GCell 101), GInstrE 'Y' (G_Read $ GCell 100), GInstrE 'Z' (G_Int 20)]
  , GAssign (GCell 100) (G_Sub (G_Read $ GCell 100) (G_Int 5))
  , GIf (G_Gt (G_Read (GCell 100)) (G_Int 0))
      (GGoto "start")
  , GLabel "end"
  , GFrame [GInstrI 'M' 100]
  , GOps [GFrame [GInstrI 'M' 30]] ""
  ] "a test program"

hcode_prog1 :: HCode ()
hcode_prog1 = do
  label "start"
  var101 <- nameCell 101
  var100 <- nameCell 100
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
    while (cur_y < 200) $ do
      count #= (count + 1)
      frame [g 100, f (speed), s (speed2), x $ cur_x, y $ cur_y] # "fast move"
      frame [g 101, z 0]
      frame [g 100, z 20] # "drilling down"
      cur_x #= (cur_x + step)
    cur_y #= (cur_y + step)


gabarit move = do
  cur_x <- nameCell 5001
  cur_y <- nameCell 5002
  cur_z <- nameCell 5003

  incr_z1 <- newVar 0
  instr_diam <- newVar 0
  visota <- newVar 0
  tolshina <- newVar 0
  number <- newVar 0
  
  frame [g 91, g 28, x 0, y 0, z 0]
  -- ...
  frame [move, {- f #150, -} y (gRead instr_diam), x (negate $ gRead instr_diam + 10)]
  -- ..
  while (gRead cur_z > (gRead visota + 15)) $ do
    frame [g 01, {- f #150, -} y (gRead instr_diam), x (negate $ gRead instr_diam + 1)]
    while (gRead cur_y > negate (gRead tolshina + gRead number - gRead instr_diam)) $ do
      y $ gRead cur_y - gRead instr_diam
    frame [y 0]
    gIf (55 > gRead cur_z) $ goto "exit"
    frame [ {- f 151, -} z $ gRead cur_z - gRead incr_z1]
  frame [z (gRead visota)]
  label "exit"
  frame [m 30]

main = do
  putStrLn "***** GCode example: \n\n"
  putGOps id gcode_prog1
  putStrLn "\n***** HCode example: \n\n"
  gcodeGen hcode_prog1

  putStrLn "\n***** HCode example2: \n\n"
  gcodeGen hcode_prog2
  
  putStrLn "\n***** Gabarit: \n\n"
  gcodeGen $ gabarit (g 0)
  
  
