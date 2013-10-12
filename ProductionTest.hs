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
  partHigh <- newVar 35.0 # "Visota zagotovoki"
  partLenth <- newVar 150.0 # "Dlina zagotovoki"
  partThickness <- newVar 50.0 # "Tolshina zagotovoki"
  numberOfparts  <- newVar (2 :: Int) # "Zagotovok v tiskah"
  comment "parametri obrabotki"
  h_oversize <- newVar 5.2 # "Pripusk po H"
  nameTool <- newVar (1 :: Int) # "instrument"
  stepXY <- newVar 25.0 # "Shag po XY"
  stepZ <- newVar 1.0 # "Shag po Z"
  feedCut <- newVar 2400 # "vrezaie"
  feedPlunge <- newVar 800 # "rezania"
  rpoinXY <- newVar 5.0 # "R-pointXY"
  spointZ <- newVar 15.0 # "S-pointZ"
  rpoinZ <- newVar 2.0 #"R-pointZ"
  comment "System variables"
  fastLinear <- newVar (1 :: Int) # "uskorenoe dvijenie: medlenno 1, bitsro 0"
  fullThickness <- newVarE $ partThickness * fi numberOfparts
  countZ <- newVar (0 :: Int) {-счётчик текущих итераций по Z-}
  cycle_type <-newVar (0::Int) {-тип текущего цилка по Z-}
  gIf (h_oversize < 2) $ do stepZ #= h_oversize / 2 {- учитываем детали с малым припуском  -}
  cur_x <- sysVar 5001
  cur_y <- sysVar 5002
  cur_z <- sysVar 5003
  cur_d <- newVarE 25.0 # "radius T"
  numStepsZ <- newVarE (fix (h_oversize / stepZ) :: Expr Int)  {- Целых шагов по Z Округлить до целых вниз #-}
  cycleZ_condition <- newVarE $ numStepsZ - fix (fi numStepsZ / 2)  {- устанавливаем условие завершения цикла по Z для первой грани #-}
  fullHigh <- newVarE $ parallelHigh + partHigh + h_oversize  {- Вычисляем полную высоту от базы тисков #-}

--пошла сама программа
  frame [g 91, g 28, z 0]  {- поднимаемся в 0 по Z -}
  frame [g 58, g 90, g 21, g 17, g 23, g 40, g 49, g 80]
  frame [m 06, t nameTool] {- Меняем инструмент на указанный в настройках  -}
  frame [m 03, s 9500] {- Запускаем шпиндель по часовой с оборотами 9500 -}
  frame [m 08] {- включаем эмульсию -}
  frame [g 00, x $ -(rpoinXY + cur_d), y $ -(rpoinXY + cur_d)]  {-Позиционируемся наверху -}
  frame [g 43, h nameTool, z $ fullHigh + spointZ ] {- активируем коррекцию по высоте заданого инструмент и опускаемся в S-point -}
  frame [g 01, z $ fullHigh, f feedCut] {-Опускаемся на уровень Z0 заготовки-}

{- Цикл обработки по Z -}
  label "spiral_cycle"
  gIf (cycleZ_condition > countZ) $ do goto "spiral_cycle_cont" {-условие цикла соблюдено-}
  gIf (cycleZ_condition < countZ) $ do goto "end_spiral_cycle" {-выходим из цикла-}
  label "spiral_cycle_cont"
  xp0 <- newVar ( 0)
  yp0 <- newVarE (-stepXY )
  xp1 <- newVarE (partLenth)
  yp1 <- newVarE (fullThickness)
  gIf (fi countZ >= stepZ) $ goto "last_spiral" {-проверяем не чистовой проход-}
  frame [g 02, r (cur_d + rpoinXY), x 0, y 0, f feedPlunge] {- врезаемся в заготовку на радиус инструмента 8-}
  frame [g 01, x xp1, f feedCut]
  label "xy_cycle" {- Цикл обработки по XY -}
  frame [g 01, x xp1 , f feedCut] {- переходим на подачу резанья по длине 1 -}
  frame [g 2, x $ xp1 + stepXY , y yp0, r  $ stepXY ] {- поворачиваем на обратный ход 2 -}
  gIf (cur_y < -(yp1 - stepXY) ) $ do goto "exit_Y40"
  frame [g 01, y $ -(yp1 - stepXY) ] {- движемся в  направлении толщины 3  -}
  frame [g 2, y $ - yp1, x xp1, r stepXY ] {- поворачиваем направо 4 -}
  gIf (cur_x < xp0 + stepXY) $ do goto "exit_X20"
  frame [g 01, x $ xp0 + stepXY ] {- движемся в  направлении длины 5  -}
  frame [g 2, x xp0, y $ - (yp1 - stepXY), r stepXY ] {- поворачиваем направо 6 -}
  gIf (cur_y > yp0 - stepXY) $ do goto "exit_Y10"
  frame [g 01, y $ yp0 - stepXY] {- движемся в  направлении толщины 7  -}
  frame [g 02, r stepXY, x $ xp0 + stepXY, y yp0, f feedCut] {- поворот на следующий круг 8-}
  xp0 #= xp0 + stepXY
  yp0 #= yp0 - stepXY
  xp1 #= xp1 - stepXY
  yp1 #= yp1 - stepXY
  gIf (cur_x > xp1) $ do goto "exit_X30"
  goto "xy_cycle" {-зацикливание на плоскости-}

  comment "exit_Y10"
  label "exit_Y10"
  frame [g 01, x $ xp1]
  frame [g 00, z $ cur_z + rpoinZ ]
  goto "end_xy_cycle"

  comment "exit_X20"
  label "exit_X20"
  frame [g 01, y $  yp0 ]
  frame [g 00, z $ cur_z + rpoinZ  ]
  goto "end_xy_cycle"

  comment "exit_X30"
  label "exit_X30"
  frame [g 01, y $ - yp1]
  frame [g 00, z $ cur_z + rpoinZ  ]
  goto "end_xy_cycle"

  comment "exit_Y40"
  label "exit_Y40"
  frame [g 01, x xp0]
  frame [g 00, z $ cur_z + rpoinZ  ]

  comment "end_xy_cycle"
  label "end_xy_cycle" {- окончание обрабоки единичной спирали  -}
  frame [g 00, x $ -(rpoinXY + cur_d), y $ -(rpoinXY + cur_d)]
  gIf (cur_z <= fullHigh) $ goto "program_end" {-проверяем не закончена ли обработка-}
  z $ cur_z - rpoinZ - stepZ {- опускаемся обатно из r-point -}
  countZ #= countZ + 1 {-увеличиваем счётчик Z-}
  xp0 #= 0
  yp0 #= -stepXY
  xp1 #= partLenth
  yp1 #= fullThickness
  goto "spiral_cycle"
  label "end_spiral_cycle"
  gIf (cycle_type ==  2) $ do goto "program_end" {-смотрим что пришли с чиствого прохода и на выход-}
  {- завершаем обработку 1й и 2й стороны-}
  z $ fullHigh + spointZ {- Поднимаемся в s-point  -}
  m 05 {- отключаем шпиндель -}
  m 09 {- отключаем эмульсию -}
  frame [g 00, x 50.0, y 240.0, z 250.0]
  m 07  {- дуем воздухом -}
  frame [g 04, p 1500]  {- ждём 1.5 сек -}
  m 09  {- отключаем воздух -}
  comment " [#3006=1] (Perevernut zagotovki cherz Y)" {- Останавливаемся и выводим сообщение -} -- FIXME

  {- Обработка второй стороны -}
  cycleZ_condition #= fix (fi numStepsZ/2)  {- устанавливаем условие завершения цикла по Z для второй грани -}
  cycle_type #= 1 {-назначаем обработку второй стороны-}
  {- Отпарвляемся на цикл спирали для второй стороны с новыми условиями для завершения целых шагов -}
  goto "spiral_cycle" {-отправляемся на цикл спирали-}

  {-последний чистовой проход устанавливающий размер-}
  label "last_spiral"
  frame [z fullHigh] {-позиционируемся по Z-}
  countZ #= 1
  cycle_type #= 2 {-назначаем последний проход-}
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
  
main = do
  putStrLn "%"
  putStrLn "O4001 (Gabarit AL  spiral 2 storoni)"
  putHCode hcode_prog2
  putStrLn "M30"
  putStrLn "\n%"