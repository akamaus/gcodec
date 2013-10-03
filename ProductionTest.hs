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
  parallelHigh <- newVar 45.97 # "Visota paraleli"
  comment "Razmeri detali"
  partHigh <- newVar 55.0 # "Visota zagotovoki"
  partLenth <- newVar 150.0 # "Dlina zagotovoki"
  partThickness <- newVar 15.0 # "Tolshina zagotovoki"
  numberOfparts  <- newVar (2 :: Int) # "Zagotovok v tiskah"
  comment "parametri obrabotki"
  h_oversize <- newVar 5.2 # "Pripusk po H"
  nameTool <- newVar (1 :: Int) # "instrument"
  stepXY <- newVar 20.0 # "Shag po XY"
  stepZ <- newVar 1.0 # "Shag po Z"
  psevdoFast <- newVar 10000   # "Uskorenaja podacha"
  feedCut <- newVar 2400 # "vrezaie"
  feedPlunge <- newVar 800 # "rezania"
  rpoinXY <- newVar 5.0 # "R-pointXY"
  spointZ <- newVar 15.0 # "S-pointZ"
  rpoinZ <- newVar 2.0 #"R-pointZ"
  comment "System variables"
  fastLinear <- newVar (1 :: Int) # "uskorenoe dvijenie: medlenno 1, bitsro 0"
  fullThickness <- newVarE $ partThickness * fi numberOfparts
  countZ <- newVar (0 :: Int)
  gIf (stepZ > h_oversize / 2) $ do stepZ #= h_oversize / 2 {- учитываем детали с малым припуском  -}
  cur_x <- sysVar 5001
  cur_y <- sysVar 5002
  cur_z <- sysVar 5003
  cur_d <- newVarE 25.0 # "radius T"
  xp0 <- newVar (0)
  yp0 <- newVarE (- cur_d)
  xp1 <- newVarE (partLenth)
  yp1 <- newVarE (fullThickness)

  numStepsZ <- newVarE $ fix $ (partHigh + h_oversize) / stepZ  {- Целых шагов по Z Округлить до целых вниз #-}
  cycleZ_condition <- newVarE (fi numStepsZ / 2)  {- устанавливаем условие завершения цикла по Z для первой грани #-}
  fullHigh <- newVarE $ parallelHigh + partHigh + h_oversize  {- Вычисляем полную высоту от базы тисков #-}

--пошла сама программа
  frame [g 91, g 28, z 0]  {- поднимаемся в 0 по Z -}
  frame [g 58, g 90, g 21, g 17, g 23, g 40, g 49, g 80]
  frame [m 06, t nameTool] {- Меняем инструмент на указанный в настройках  -}
  frame [m 03, s 9500] {- Запускаем шпиндель по часовой с оборотами 9500 -}
  frame [m 08] {- включаем эмульсию -}
  frame [g fastLinear, x $ -(rpoinXY + cur_d), y $ -(rpoinXY + cur_d), f psevdoFast]  {-Позиционируемся наверху -}
  frame [g 43, h nameTool, z $ fullHigh + spointZ ] {- активируем коррекцию по высоте заданого инструмент и опускаемся в S-point -}
  frame [g 01, z $ fullHigh, f feedCut] {-Опускаемся на уровень Z0 заготовки-}

{- Цикл обработки по Z -}
  label "spiral_cycle"
  while (cycleZ_condition > fi countZ) $ do

    gIf (fi countZ >= stepZ) $ goto "last_spiral" {-проверяем не чистовой пришла и пора чистового прохода-}
    frame [g 02, r $ cur_d + rpoinXY, x 0, y 0, f feedPlunge] {- врезаемся в заготовку на радиус инструмента 8-}
    frame [g 01, x xp0, f feedCut]
    while (xp1 > cur_d) $ do {- Цикл обработки по XY -}

      frame [g 01, x xp1 , f feedCut] {- переходим на подачу резанья по длине 1 -}
      frame [g 2, x xp1, y $ - yp1, r stepXY ] {- поворачиваем на обратный ход 2 -}
      gIf (cur_y < yp1 - stepXY ) $ do goto "exit_Y40"
      frame [g 01, y $ yp1 - stepXY ] {- движемся в  направлении толщины 3  -}
      frame [g 2, y $ - yp1, x xp1, r stepXY ] {- поворачиваем направо 4 -}
      gIf (cur_x < xp0 + stepXY) $ do goto "exit_X20"
      frame [g 01, x $ xp0 + stepXY ] {- движемся в  направлении длины 5  -}
      frame [g 2, x xp0, y $ - yp1 - stepXY, r stepXY ] {- поворачиваем направо 6 -}
      gIf (cur_y > yp0 - stepXY) $ do goto "exit_Y10"
      frame [g 01, y $ yp0 - stepXY] {- движемся в  направлении толщины 7  -}
      frame [g 02, r stepXY, x $ xp0 + stepXY, y yp0, f feedCut] {- поворот на следующий круг 8-}
      xp0 #= xp0 + stepXY
      yp0 #= yp0 - stepXY
      xp1 #= xp1 - stepXY
      yp1 #= yp1 - stepXY
      gIf (cur_x > xp1) $ do goto "exit_X30"

  label "exit_Y1"
  frame [g 01, z $ cur_z + rpoinZ ]
  frame [g 00, x $ - cur_d]
  frame [g 00, y $ - (stepXY + rpoinXY)] -- FIXME висел плюс
  goto "end_xy_cycle"

  label "exit_X"
  frame [g 01, z $ cur_z + rpoinZ  ]
  frame [g 00, y $ cur_d + rpoinXY]
  goto "end_xy_cycle"

  label "exit_Y10"
  frame [g 01, z $ cur_z + rpoinZ  ]
  frame [g 00, x partLenth]
  y $ cur_d + rpoinXY

  label "end_xy_cycle" {- окончание обрабоки единичной спирали  -}
  gIf (cur_z <= fullHigh) $ goto "program_end" {-проверяем не закончена ли обработка-}
  z $ cur_z - rpoinZ - stepZ {- опускаемся обатно из r-point -}
  countZ #= countZ + 1


  {- завершаем обработку 1й и 2й стороны-}
  z $ fullHigh + spointZ {- Поднимаемся в s-point  -}
  m 05 {- отключаем шпиндель -}
  m 09 {- отключаем эмульсию -}
  frame [g 00, x 50.0, y 240.0, z 250.0]
  m 07  {- дуем воздухом -}
  frame [g 04, p 1500]  {- ждём 1.5 сек -}
  m 09  {- отключаем воздух -}
  m 00
  -- FIXME
  --  frame [#3006=1] # "Perevernut zagotovki cherz Y" {- Останавливаемся и выводим сообщение -}

  {- Обработка второй стороны -}
  cycleZ_condition <- newVarE (fi numStepsZ)  {- устанавливаем условие завершения цикла по Z для второй грани #-}
  {- Отпарвляемся на цикл спирали для второй стороны с новыми условиями для завершения целых шагов -}
  goto "spiral_cycle"

  {-последний чистовой проход устанавливающий размер-}
  label "last_spiral"
  frame [z fullHigh] {-позиционируемся по Z-}
  countZ #= countZ - 1
  goto "spiral_cycle" {-отправляемся на цикл спирали-}

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
  putStrLn "\n***** Production: \n\n"
  putHCode hcode_prog2
