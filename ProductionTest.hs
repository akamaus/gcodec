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
  parallelHigh <- newVar (45.97 :: Double) # "Visota paraleli"
  comment "Razmeri detali"
  partHigh <- newVar (55.0 :: Double) # "Visota zagotovoki"
  partLenth <- newVar (150.0 :: Double) # "Dlina zagotovoki"
  partThickness <- newVar (15.0 :: Double) # "Tolshina zagotovoki"
  numberOfparts  <- newVar (2 :: Int) # "Zagotovok v tiskah"
  comment "parametri obrabotki"
  h_oversize <- newVar (5.2 :: Double) # "Pripusk po H"
  nameTool <- newVar (1 :: Int) # "instrument"
  stepXY <- newVar (20.0 :: Double) # "Shag po XY"
  stepZ <- newVar (1.0 :: Double) # "Shag po Z"
  psevdoFast <- newVar (10000 :: Double)  # "Uskorenaja podacha"
  feedCut <- newVar 2400 # "vrezaie"
  feedPlunge <- newVar 800 # "rezania"
  rpoinXY <- newVar (5.0 :: Double) # "R-pointXY"
  spointZ <- newVar (15.0 :: Double) # "S-pointZ"
  rpoinZ <- newVar (2.0 : Double) #"R-pointZ"
  comment "System variables"
  fastLinear <- newVar (1 :: Int) # "uskorenoe dvijenie: medlenno 1, bitsro 0"
  fullThickness <- newVar (partThickness * numberOfparts : Double)
  countZ <- newVar (0 :: Int)
  gIf (stepZ > h_oversize / 2) $ do stepZ = h_oversize / 2 {- учитываем детали с малым припуском  -}
  xp0 <- newVar (0)
  yp0 <- newVar (-cur_d)
  xp1 <- newVar (partLenth)
  yp1 <- newVar (fullThickness)
  cur_x <- sysVar 5001
  cur_y <- sysVar 5002
  cur_z <- sysVar 5003
  cur_d <- newVarE (25.0 :: Double) #"radius T"
  numStepsZ <- newVarE $ fix $ (partHigh + h_oversize) / stepZ  {- Целых шагов по Z Округлить до целых вниз #-}
  cycleZ_condition <- newVar (fi numStepsZ / 2)  {- устанавливаем условие завершения цикла по Z для первой грани #-}
  fullHigh #= parallelHigh + partHigh + h_oversize  {- Вычисляем полную высоту от базы тисков #-}

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
  while (cycleZ_condition  > fi countZ) $ do

    gIf (countZ >= stepZ) $ goto last_spiral {-проверяем не чистовой пришла и пора чистового прохода-}
    frame [g 02, r cur_d + rpoinXY, x 0, y 0, f feedPlunge] {- врезаемся в заготовку на радиус инструмента 8-}
    frame [g 01, x xp0, f feedCut]
    while (xp1 > cur_d) $ do {- Цикл обработки по XY -}

      frame [g 01, x xp1 , f  feedCut] {- переходим на подачу резанья по длине 1 -}
      frame [g 2, x xp1, y - yp1, r stepXY ] {- поворачиваем на обратный ход 2 -}
      gIf (cur_y < yp1 - stepXY ) $ do goto exit_Y40
      frame [g 01, y $ yp1 - stepXY ] {- движемся в  направлении толщины 3  -}
      frame [g 2, y - yp1, x xp1, r stepXY ] {- поворачиваем направо 4 -}
      gIf (cur_x < xp0 + stepXY) $ do goto exit_X20
      frame [g 01, x xp0 + stepXY ] {- движемся в  направлении длины 5  -}
      frame [g 2, x xp0, y - yp1 - stepXY, r stepXY ] {- поворачиваем направо 6 -}
      gIf (cur_y > yp0 - stepXY) $ do goto exit_Y10
      frame [g 01, y yp0 - stepXY] {- движемся в  направлении толщины 7  -}
      frame [g 02, r stepXY, x xp0 + stepXY, y yp0, f feedCut] {- поворот на следующий круг 8-}
      xp0 = xp0 + stepXY
      yp0 = yp0 - stepXY
      xp1 = xp1 - stepXY
      yp1 = yp1 - stepXY
      gIf (cur_x > xp1) $ do goto exit_X30

  label "exit_Y1"
  frame [g 01, z cur_z + rpoinZ  ]
  frame [g 00, x - cur_d]
  frame [g 00, y - (stepXY + rpoinXY + )]
  goto end_xy_cycle

  label "exit_X"
  frame [g 01, z cur_z + rpoinZ  ]
  frame [g 00, y cur_d + rpoinXY]
  goto end_xy_cycle

  label "exit_Y10"
  frame [g 01, z cur_z + rpoinZ  ]
  frame [g 00, x partLenth]
  frame [y cur_d + rpoinXY]

 label "end_xy_cycle" {- окончание обрабоки единичной спирали  -}
 gIf (cur_z <= fullHigh) $ goto program_end {-проверяем не закончена ли обработка-}
 frame [z cur_z - rpoinZ - stepZ  ] {- опускаемся обатно из r-point -}
 countZ = countZ + 1


{- завершаем обработку 1й и 2й стороны-}
  z $ fullHigh + spointZ {- Поднимаемся в s-point  -}
  frame [m 05] {- отключаем шпиндель -}
  frame [m 09] {- отключаем эмульсию -}
  frame [g 00, x 50.0, y 240.0, z 250.0]
  frame [m 07]  {- дуем воздухом -}
  frame [g 04, p 1500]  {- ждём 1.5 сек -}
  frame [m 09]  {- отключаем воздух -}
frame [m00]
-- FIXME
--  frame [#3006=1] # "Perevernut zagotovki cherz Y" {- Останавливаемся и выводим сообщение -}

{- Обработка второй стороны -}
cycleZ_condition <- newVar (fi numStepsZ)  {- устанавливаем условие завершения цикла по Z для второй грани #-}
{- Отпарвляемся на цикл спирали для второй стороны с новыми условиями для завершения целых шагов -}
goto spiral_cycle

{-последний чистовой проход устанавливающий размер-}
label "last_spiral"
frame [z fullHigh] {-позиционируемся по Z-}
countZ = countZ - 1
goto spiral_cycle {-отправляемся на цикл спирали-}

{- Завершение программы -}
label "program_end"
  z $ fullHigh + spointZ {- Поднимаемся в s-point  -}
  frame [m 05] {- отключаем шпиндель -}
  frame [m 09] {- отключаем эмульсию -}
  frame [g 00, x 50.0, y 240.0, z 250.0]
  frame [m 07]  {- дуем воздухом -}
  frame [g 04, p 1500]  {- ждём 1.5 сек -}
  frame [m 09]  {- отключаем воздух -}
main = do
  putStrLn "\n***** Production: \n\n"
  gcodeGen hcode_prog2
