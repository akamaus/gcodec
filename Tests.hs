{-# LANGUAGE OverloadedStrings #-}

import GOperator
import GCode

gcode_prog1 = GOps
  [ GLabel "start"
  , GAssign (GCell 101) (G_Add (G_Read $ GCell 101) (G_Int 42))
  , GFrame [G 1, X (G_Read $ GCell 101), Y (G_Read $ GCell 100), Z (G_Int 20)]
  , GAssign (GCell 100) (G_Sub (G_Read $ GCell 100) (G_Int 5))
  , GIf (G_Gt (G_Read (GCell 100)) (G_Int 0))
      (GGoto "start")
      Nothing
  , GLabel "end"
  , GFrame [M 100]
  ]

hcode_prog1 :: GCode ()
hcode_prog1 = do
  label "start"
  var101 <- nameCell 101
  var100 <- nameCell 100
  var101 #= (gRead var101 + 42)
  frame [g 1, z (gRead var101), y (gRead var100), z 20]
  var100 #= (gRead var100 - 5)
  gIf (gRead var100 #> 0)
    (goto "start")
    (return ())
  label "end"
  m 100

main = do
  putStrLn "GCode example: \n\n"
  putGOps gcode_prog1
  putStrLn "\nHCode example: \n\n"
  gcodeGen hcode_prog1