{-# LANGUAGE OverloadedStrings #-}

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
  partLenth <- newVar 244.0 # "Dlina zagotovoki"
  partThickness <- newVar 28.0 # "Tolshina zagotovoki"
  terrace_width <- newVar 20.0 #"shirina ustupa"
  terrace_high <- newVar 45.8 #"visot ustupa"
  angle_near_wall <- newVar 30 #"ugol okolo stenki"
  near_wall_high <- 1.5 #"uglublenie okolo stenki"
  near_wall_width <- 3.0 #"ruchei u stenki"
  comment "parametri obrabotki"
  allowance <- newVar 0.5 #"pripusk XY, Z"
  rpoinXY <- newVar 5.0 # "R-pointXY"
  spointZ <- newVar 15.0 # "S-pointZ"
  rpoinZ <- newVar 2.0 #"R-pointZ"
  comment "System variables"
  fullThickness <- newVarE $ partThickness * 2
  cur_x <- sysVar 5001
  cur_y <- sysVar 5002
  cur_z <- sysVar 5003
  fullHigh <- newVarE $ parallelHigh + partHigh  {- Вычисляем полную высоту от базы тисков #-}

  --начало самой программы
  frame [g 91, g 28, z 0]  {- поднимаемся в 0 по Z -}
  frame [g 58, g 90, g 21, g 17, g 23, g 40, g 49, g 80]
  frame [m 06, t 1]
  frame [m 03, s 6500] {- Запускаем шпиндель по часовой с оборотами 6500 -}
  frame [m 08] {- включаем эмульсию -}
  frame [g 00, x $ -(rpoinXY + cur_d), y $ -(rpoinXY + cur_d * 3)]  {-Позиционируемся наверху -}
  frame [g 43, h nameTool, z $ fullHigh + spointZ ] {- активируем коррекцию по высоте заданого инструмент и опускаемся в S-point -}
  frame [g 01, z $ fullHigh, f feedCut] {-Опускаемся на уровень Z0 заготовки-}

  -- Черновой Цикл обработки первой детали
  stepZ #= 1
  cur_cycle_steps #= fix $ (partHigh - terrace_high) / stepZ
  feedCut <- newVar 2400 # "vrezaie"
  feedPlunge <- newVar 800 # "rezania"

  label "first_cycle_part1_start" --собственно сам первый черновой цикл
  gIf (countZ  => cur_cycle_steps) $ goto "first_cycle_part1_end"
  g 41, d 1, y $ -terrace_width + allowance
  g 01, x cur_d, feedPlunge
  x $ partLenth + rpoinXY,f feedCut
  g 40, y $ rpoinXY + cur_d * 3
  g 00, x $ - (cur_d + rpoinXY)
  countZ #= countZ + 1
  z $ cur_z - stepZ
  goto "first_cycle_part1_start" --зацикливаем
  label "first_cycle_part1_end"
  -- последний проход по первой детали
    z $ fullHigh - (partHigh - terrace_high) -- позиционируемся на финальную высоту
    g 41, d 1, y $ -terrace_width + allowance
    g 01, x cur_d, feedPlunge
    x $ partLenth + rpoinXY,f feedCut
    g 40,g 00, y $ cur_d + rpoinXY
    x $ - (cur_d + rpoinXY)
    z $ fullHigh + spointZ -- поднимаемся на безопасную высоту
  -- переходим ко второй детали
  countZ #= 0 --сбрасываем счётчик цикла
  x $ partLenth + rpoinXY + cur_d, y $ - (partThickness * 3 + cur_d + rpoinXY) --позиционируемся сверху
  g 01, z fullHigh, f feedCut --опускаемся в уровень с верхом заготовки

  label "first_cycle_part2_start" -- начинаем вторую деталь
   gIf (countZ  => cur_cycle_steps) $ do goto "first_cycle_part2_end"
   g 41, d 1, y $ - (partThickness + terrace_width + allowance )
   g 01, x partLenth, f feedPlunge
   x $ - (cur_d + rpoinXY)
   g 00, y $ - (partThickness * 3 + cur_d + rpoinXY
   x $ partLenth + rpoinXY + cur_d
   countZ #= countZ + 1
   z $ cur_z - stepZ
  goto "first_cycle_part2_start" --зацикливаем
  label "first_cycle_part2_end"
  -- последний проход по первой детали
   z $ fullHigh - (partHigh - terrace_high) -- позиционируемся на финальную высоту
   g 41, d 1, y $ - (partThickness + terrace_width + allowance )
   g 01, x partLenth, f feedPlunge
   x $ - (cur_d + rpoinXY)
   g 00, y $ - (partThickness * 3 + cur_d + rpoinXY
   x $ partLenth + rpoinXY + cur_d
  {- завершаем черновую обработку 1й и 2й детали-}
  z $ fullHigh + spointZ {- Поднимаемся в s-point  -}
  m 05 {- отключаем шпиндель -}
  m 09 {- отключаем эмульсию -}
  --чистовой проход фрезой D10
  m 06, t 15
  g 00,g 43,h 15, z $ fullHigh + 150.0 --позиционируемся над деталью на высоте 150мм
  -- первая деталь
  g 00, x $ -(rpoinXY + cur_d), y $ -(rpoinXY + cur_d * 3)  {-Позиционируемся у 1й детали по XY -}
  m 08
  m 03, s 10500
  -- пошла обработка 1й детали
  g 01, z fullHigh, f feedCut --опускаемся в уровень с верхом заготовки
  g 41, d 15, y $ -terrace_width
  g 01, x cur_d, feedPlunge
  x $ partLenth + rpoinXY,f feedCut
  g 40, y $ rpoinXY + cur_d * 3
  {- Завершение программы -}
  label "program_end"
  z $ fullHigh + spointZ {- Поднимаемся в s-point  -}
  frame [m 05] {- отключаем шпиндель -}
  frame [m 09] {- отключаем эмульсию -}
  frame [g 00, x 50.0, y 240.0, z 250.0]
  frame [m 07]  {- дуем воздухом -}
  frame [g 04, p 1500]  {- ждём 1.5 сек -}
  frame [m 09]  {- отключаем воздух -}

-- МУСОРНЫЕ лейблы
  label "exit_X20"
  label "exit_X30"
  label "exit_Y40"

main = do
  putStrLn "%"
  putStrLn "O4201 (culaga universal Exprof)"
  putHCode hcode_prog2
  putStrLn "\n%"