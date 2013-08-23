{-# LANGUAGE OverloadedStrings #-}

import GOperator

prog1 = GOps
  [ GLabel "start"
  , GAssign (GCell 101) (G_Add (G_Read $ GCell 101) (G_Int 42))
  , GFrame [G 1, X (G_Read $ GCell 101), Y (G_Read $ GCell 100), Z (G_Int 20)]
  , GAssign (GCell 100) (G_Sub (G_Read $ GCell 100) (G_Int 5))
  , GIf (G_GT (G_Read (GCell 100)) (G_Int 0))
      (GGoto "start")
      Nothing
  , GLabel "end"
  , GFrame [M 100]
  ]

main = do
  putGOps prog1
