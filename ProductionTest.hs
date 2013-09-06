{-# LANGUAGE OverloadedStrings #-}

import GCode
import HCode

import AwePrelude
import Prelude(Num(..), Fractional(..), Floating(..), Int, Double, ($), id, putStrLn, return)

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

comment str = (return ()) # str

hcode_prog2 :: HCode ()
hcode_prog2 = do
-- блок переменных
  parallelHigh <- newVar (45.97 :: Double) # "Visota paraleli"
  comment "Razmeri detali"
  partHigh <- newVar (55.0 :: Double) # "Visota zagotovoki"
  partLenth <- newVar (150.0 :: Double) # "Dlina zagotovoki"
  partThickness <- newVar (15.0 :: Double) # "Tolshina zagotovoki"
  numberOfparts  <- newVar (2 :: Int) # "Tolshina zagotovoki"
  comment "parametri obrabotki"
  h_oversize <- newVar (5.2 :: Double) # "Pripusk po H"
  nameTool <- newVar (1 :: Int) # "instrument"
  stepXY <- newVar (25.0 :: Double) # "Shag po XY"
  stepZ <- newVar (1.0 :: Double) # "Shag po Z"
  psevdoFast <- newVar (15000 :: Double)  # "Uskorenaja podacha"
  feedCut <- newVar 2400 # "vrezaie"
  feedPlunge <- newVar 800 # "rezania"
  rpoinXY <- newVar (5.0 :: Double) # "R-pointXY"
  spointZ <- newVar (15.0 :: Double) # "R-pointZ"
  comment "System variables"
  fastLinear <- newVar (1 :: Int) # "uskorenoe dvijenie: medlenno 1, bitsro 0"
  countXY <- newVar (0 :: Int)
  countZ <- newVar (0 :: Int)
  cur_x <- nameCell 5001
  cur_y <- nameCell 5002
  cur_z <- nameCell 5003
  cur_d <- nameCell 2001
--  cur_d <- nameCell _OFS[nameTool] FIXME
  fullHigh <- newVar (200.0 :: Double)
  numStepsXY <- newVarE $ fup $ parallelHigh + partHigh + h_oversize {- Целых шагов по XY округлить до целых вверх #-}
  numStepsZ <- newVarE $ fix $ (partHigh + h_oversize) / stepZ  {- Целых шагов по Z Округлить до целых вниз #-}
  fullHigh #= parallelHigh + partHigh + h_oversize  {- Вычисляем полную высоту от базы тисков #-}

--пошла сама программа
  frame [g 91, g 28, z 0]  {- поднимаемся в 0 по Z -}
  frame [g 58, g 90, g 21, g 17, g 23, g 40, g 49, g 80]
  frame [g fastLinear, x $ -(rpoinXY + cur_d), y $ (rpoinXY + cur_d), f psevdoFast]  {-Позиционируемся наверху -}
  frame [m 06, t nameTool] {- Меняем инструмент на указанный в настройках  -}
  frame [m 03, s 9500] {- Запускаем шпиндель по часовой с оборотами 9500 -}
  frame [m 08] {- включаем эмульсию -}
  frame [g 43, h nameTool, z $ fullHigh + spointZ ] {- активируем коррекцию по высоте заданого инструмент и опускаемся в S-point -}
  frame [g 01, z $ fullHigh, f feedCut] {-Опускаемся на уровень Z0 заготовки-}

  while (fi numStepsZ / 2 > fi countZ) $ do {- Цикл обработки по Z -}
    while (numStepsXY > countXY) $ do {- Цикл обработки по XY -}
      frame [g 01, x cur_d, f $ feedPlunge] {- врезаемся в заготовку на радиус инструмента-}
      frame [g 01, x partLenth, f $ feedCut] {- переходим на подачу резанья -}
      frame [g fastLinear, x $ -(rpoinXY + cur_d), f $ psevdoFast ] {- возвращаемся назад по X -}
      frame [y $ cur_y - stepXY] {- Сдвигаемся на шаг по Y  -}
      countXY #= countXY + 1
  frame [y $ (rpoinXY + cur_d)] {-  возвращаемся в начало по цикла XY -}
  frame [z $ cur_z - stepZ] {- Опускаемся на шаг по Z  -}
  countZ #= countZ + 1

  z $ fullHigh + spointZ {- Поднимаемся в s-point  -}
  frame [m 05] {- отключаем шпиндель -}
  frame [m 09] {- отключаем эмульсию -}
  frame [g fastLinear, x 50.0, y 240.0, z 250.0, f psevdoFast]
  frame [m 07]  {- дуем воздухом -}
  frame [g 04, p 1500]  {- ждём 1.5 сек -}
  frame [m 09]  {- отключаем воздух -}

-- FIXME
--  frame [#3006=1] # "Perevernut zagotovki cherz Y" {- Останавливаемся и выводим сообщение -}

  frame [m 03, s 9500] {- Запускаем шпиндель по часовой с оборотами 9500 -}
  frame [m 08] {- включаем эмульсию -}
  frame [g 43, h $ nameTool, z $ fullHigh + spointZ ] {- активируем коррекцию по высоте заданого инструмент и опускаемся в S-point -}
  frame [g 01, z $ fullHigh - fi numStepsZ/2 , f feedCut] {-Опускаемся на уровень Z0 заготовки-}

  while (fi numStepsZ/2 > fi countZ) $ do {- Цикл обработки по Z -}
    while (numStepsXY > countXY) $ do {- Цикл обработки по XY -}
      frame [g 01, x $ cur_d, f $ feedPlunge] {- врезаемся в заготовку на радиус инструмента-}
      frame [g 01, x $ partLenth, f $ feedCut] {- переходим на подачу резанья -}
      frame [g fastLinear, x $ -(rpoinXY + cur_d), f $ psevdoFast ] {- возвращаемся назад по X -}
      frame [y $ cur_y - stepXY] {- Сдвигаемся на шаг по Y  -}
      countXY #= countXY + 1
  frame [y $ (rpoinXY + cur_d)] {-  возвращаемся в начало по цикла XY -}
  frame [z $ cur_z - stepZ] {- Опускаемся на шаг по Z  -}
  countZ #= countZ + 1

  frame [z $ fullHigh + spointZ ] {- Поднимаемся в s-point  -}



main = do
  putStrLn "***** GCode example: \n\n"
  putGOps id gcode_prog1
  putStrLn "\n***** HCode example: \n\n"
  gcodeGen hcode_prog1

  putStrLn "\n***** HCode example2: \n\n"
  gcodeGen hcode_prog2
