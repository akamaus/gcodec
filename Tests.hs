{-# LANGUAGE OverloadedStrings #-}

import GCode
import HCode

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
  gIf (var100 #> 0)
    (goto "start")
  m 30
  label "end"

hcode_prog2 :: HCode ()
hcode_prog2 = do
  let safe = 20
      step = 15
  frame [g 100, z safe]
  speed <- newVar 15
  speed2 <- newVar 2
  cur_x <- newVar 5.0
  cur_y <- newVar 5.0
  count <- newVar (0 :: Int)
  while (100 #> cur_x) $ do
    while (200 #> cur_y) $ do
      count #= (count + 1)
      frame [g 100, f (speed), s (speed2), x $ cur_x, y $ cur_y]
      frame [g 101, z 0]
      frame [g 100, z 20]
      cur_x #= (cur_x + step)
    cur_y #= (cur_y + step)

main = do
  putStrLn "***** GCode example: \n\n"
  putGOps id gcode_prog1
  putStrLn "\n***** HCode example: \n\n"
  gcodeGen hcode_prog1

  putStrLn "\n***** HCode example2: \n\n"
  gcodeGen hcode_prog2
