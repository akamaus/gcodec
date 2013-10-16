{-# LANGUAGE OverloadedStrings, ImplicitParams #-}
import GCode
import HCode

import AwePrelude
import Prelude(Num(..), Fractional(..), Floating(..), Int, Double, ($), id, putStrLn, return)

comment str = (return ()) # str

hcode_prog2 :: HCode ()
hcode_prog2 = do
-- блок переменных
  ofs_table <- sysTable "_OFS"
  parallelHigh <- newVar 45.97 # "Visota paralelii"
  comment "Razmeri detali"
  partHigh <- newVar 57.0 # "Visota zagotovoki"
--  partLength <- newVar 244.0 # "Dlina zagotovoki"
--  partThickness <- newVar 28.0 # "Tolshina zagotovoki"
 
  comment "paz 1 vdol X"
  fx_slot_x <- newVar 60.0 # "do centra 1 paza po X"
  fx_slot_y <- newVar 14.5 # "do centra 1 paza po Y"
{-  fx_slot_length <- newVar 40.0 # "dlina 1 paza"
  fx_slot_depth  <- newVar 10.0 # "glubina 1 paza storona 1"
  fx_slot_depth_2  <- newVar 0.0 # "glubina 1 paza storona 2"
  comment "paz 2 vdol y"
  fy_slot_x <- newVar 60.0 # "do centra 2 paza po X"
  fy_slot_y <- newVar 14.5 # "do centra 2 paza po Y"
  fy_slot_length <- newVar 40.0 # "dlina 2 paza"
  fy_slot_depth  <- newVar 0.0 # "glubina 2 paza storona 1"
  fy_slot_depth_2  <- newVar 18.0 # "glubina 2 paza storona 2"
  comment "R pazov"
  fx_slot_ext_R <- newVar 7.0 # "1 paz po x snaruji R"
  fx_slot_int_R <- newVar 4.5 # "1 paz po x vnutri R"
  fy_slot_ext_R <- newVar 7.0 # "2 paz po y snaruji R"
  fy_slot_int_R <- newVar 4.5 # "2 paz po y vnutri R"-}

  comment "parametri obrabotki"
--  rpoinXY <- newVar 5.0 # "R-pointXY"
  spointZ <- newVar 15.0 # "S-pointZ"
--  rpointZ <- newVar 2.0 #"R-pointZ"
--  nameTool <- newVar (1 :: Int) # "instrument"
--  stepZ <- newVar (1 :: Double) # "shag po Z"
--  feedCut <- newVar 2400 # "vrezaie"
--  feedPlunge <- newVar 800 # "rezania"
--  cur_d <- newVar (0 :: Double) # "R-instrumenta"
  comment "System variables"
  cur_x <- sysVar 5001
  cur_y <- sysVar 5002
  cur_z <- sysVar 5003
  fullHigh <- newVarE $ parallelHigh + partHigh  {- Вычисляем полную высоту от базы тисков #-}
--только функции SDL
--  let ?rpoinXY = rpoinXY
--  let ?spointZ = spointZ
--  let ?rpointZ = rpointZ
--  let ?feedCut = feedCut
--  let ?feedPlunge = feedPlunge
--  let ?cur_d = cur_d
--  let ?nameTool = nameTool
--  let ?cur_z = cur_z
--  let ?cur_y = cur_y
--  let ?cur_x = cur_x
  let ?fullHigh = fullHigh
--пошла сама программа
  let ?spointZ = spointZ
  frame [g 91, g 28, z 0] -- поднимаемся в референтную позицию по Z
  frame [g 58, g 90, g 21, g 17, g 23, g 40, g 49, g 80] --строка безопасности
-- черновая обработка уступов 2х деталей
-- инициализация параметров обработки
  change_tool_start 1 fullHigh 8000 -- устанавливаем инструмент
  slot_preposition_XY fx_slot_x fx_slot_y -- позиционируемся на паз
  --mill_x_slot fx_slot_length fx_slot_depth
  operation_end fullHigh
  m 30




 --объявленные функции
-- фрезерование паза в праралельно X
{-mill_x_slot slot_length slot_depth slotR = do
    frame [g 01, z ?fullHigh + ?spointZ, f 10000]
    frame [z $ ?fullHigh, f ?feedCut]
    frame [g 41,d ?nameTool, y $ ?cur_y - slotR] -- выходим на линию реза
    steps <- newVarE $ fix (slot_depth / ?stepZ)
    while (steps > 0) $ do
      steps #= steps - 1.0
      frame [g 01, x $ ?cur_x + ?cur_d, z $ ?cur_z - ?stepZ, ?feedPlunge]
      mill_x_one_iteration slotR slot_length
    frame [g 01, x $ ?cur_x + ?cur_d, z $ ?fullHigh - slot_depth, ?feedPlunge] -- финишная высота
    mill_x_one_iteration slotR slot_length -- финишный круг
    g 40


-- фрезерование одного круга в направлении Х
mill_x_one_iteration  slotR slot_length = do
    frame [x $ ?cur_x + slot_length - ?cur_d]
    frame [y $ ?cur_y + slotR * 2]
    frame [x $ ?cur_x + slot_length - ?cur_d * 2]
    frame [y $ ?cur_y - slotR * 2]
-}
-- позиционирование над пазом
slot_preposition_XY slot_start_x slot_start_y = do
  frame [g 00, x slot_start_x, y slot_start_y]

-- смена инструмента
change_tool_start nameTool fullHigh s_spindel = do
  frame [m 06, t nameTool]
  frame [g 00, g 43, h nameTool, z $ fullHigh + 150.0 ]
  m 08
  frame [m 03, s s_spindel]

-- завершение операции
iteration_end fullHigh = do
  z $ fullHigh + ?spointZ {- Поднимаемся в s-point  -}
  frame [m 05] {- отключаем шпиндель -}
  frame [m 09] {- отключаем эмульсию -}

operation_end fullHigh  = do
  z $ fullHigh + ?spointZ {- Поднимаемся в s-point  -}
  frame [m 05] {- отключаем шпиндель -}
  frame [m 09] {- отключаем эмульсию -}
  frame [g 00, x 50.0, y 240.0, z 250.0]
  frame [m 07]  {- дуем воздухом -}
  frame [g 04, p 1500]  {- ждём 1.5 сек -}
  frame [m 09]  {- отключаем воздух -}


main = do
  putStrLn "%"
  putStrLn "O4202 (pazi universal _|)"
  putHCode hcode_prog2
  putStrLn "\n%"