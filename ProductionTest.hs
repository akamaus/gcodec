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
  parallelHigh <- newVar 39.95 # "Visota paralelii"
  comment "Razmeri detali"
  partHigh <- newVar 40.0 # "Visota zagotovoki"
  partLenth <- newVar 115.0 # "Dlina zagotovoki"
  partThickness <- newVar 24.0 # "Tolshina zagotovoki"
  numberOfparts  <- newVar (2 :: Int) # "Zagotovok v tiskah"
  comment "parametri obrabotki"
  h_oversize <- newVar 7.0 # "Pripusk po H"
  nameTool <- newVar (1 :: Int) # "instrument"
  stepXY <- newVar (15.0 :: Double) # "Shag po XY"
  stepZ <- newVar 1.0 # "Shag po Z"
  feedCut <- newVar (1200 :: Double)# "vrezaie"
  feedPlunge <- newVar 3800 # "rezania"
  rpoinXY <- newVar 5.0 # "R-pointXY"
  spointZ <- newVar 15.0 # "S-pointZ"
  rpoinZ <- newVar 2.0 #"R-pointZ"
  comment "System variables"
  eps <- newVar (0.0001 :: Double) #"epsilon"
  fullThickness <- newVarE $ partThickness * fi numberOfparts
  cycle_type <-newVar (0::Int) {-тип текущего цилка по Z-}
  gIf (h_oversize <= 2) $ do stepZ #= h_oversize / 2 {- учитываем детали с малым припуском  -}
  cur_d <- newVarE 25.0 # "radius T"
  numStepsZ <- newVarE (fix (h_oversize / stepZ) :: Expr Int)  {- Целых шагов по Z Округлить до целых вниз #-}
  cycleZ_main_condition <- newVarE $ numStepsZ - fix (fi numStepsZ / 2)  {- устанавливаем условие завершения цикла по Z для первой грани #-}
  fullHigh <- newVarE $ parallelHigh + partHigh + h_oversize  {- Вычисляем полную высоту от базы тисков #-}
--только SLD функции
  let xy_cycle = spiral_XY stepXY partLenth fullThickness
  let z_cycle = z_one_iteration_cycle xy_cycle rpoinXY cur_d rpoinZ stepZ cycleZ_main_condition feedPlunge
  let process_side proc_acts = do
        --предварительное позиционирование XY, опускаемся на 150мм от заготовки
        prepare_preposition_block nameTool rpoinXY cur_d fullHigh rpoinZ
        -- основной цикл обработки
        proc_acts
        --завершаем операцию
        operation_end fullHigh spointZ
  let finishing = do
        gIf ( h_oversize - fi numStepsZ * stepZ  > eps) $ do
          z $ parallelHigh + partHigh --позиционируемся на финальную выстоу
          frame [g 02, r (cur_d + rpoinXY), x 0, y 0, f feedPlunge] -- врезаемся в заготовку на радиус инструмента
          xy_cycle -- вызываем спираль

--пошла сама программа
  frame [g 91, g 28, z 0] -- поднимаемся в референтную позицию по Z
  frame [g 58, g 90, g 21, g 17, g 23, g 40, g 49, g 80] --строка безопасности
  frame [m 06, t nameTool] -- Меняем инструмент на указанный в настройках

  process_side z_cycle

  comment " [#3006=1] (Perevernut zagotovki cherz Y)" ---Останавливаемся и выводим сообщение !FIXME!

--Обработка второй стороны
  --устанавливаем новую полную высоту после обработки первой стороны
  fullHigh #= parallelHigh + (partHigh + h_oversize - fi (cycleZ_main_condition) * stepZ)
  -- устанавливаем количество целых шагов цикла Z для второй стороны
  cycleZ_main_condition #= fix (fi numStepsZ/2)
  -- переходим к обработке второй стороны
  process_side $ do
    z_cycle --целые шаги
    finishing -- чистовая обработка

  -- выходим из программы
  m 30

-- объявление функций
--функция основного цикла по Z, включает в себя позиционирование и врезание
z_one_iteration_cycle xy_cycle rpoinXY cur_d rpoinZ stepZ cycleZ_main_condition feedPlunge  = do
  cur_z <- sysVar 5003 --инициализируем системную переменную Z на конец предыдущего блока
  countZ <- newVar (0 :: Int) {-счётчик текущих итераций по Z-}

  while (cycleZ_main_condition > countZ) $ do --Цикл обработки по Z
    z $ cur_z - rpoinZ - stepZ --опускаемся в плоскость резания из r-point
    countZ #= countZ + 1 --увеличиваем счётчик Z
    frame [g 02, r (cur_d + rpoinXY), x 0, y 0, f feedPlunge] -- врезаемся в заготовку на радиус инструмента
    xy_cycle -- вызываем спираль XY
    frame [g 00, z $ cur_z + rpoinZ] --поднимаемся в Z r-point
    frame [x $ -(rpoinXY + cur_d), y $ -(rpoinXY + cur_d)] -- позиционируемся на Z (R-point) для нового захода


--функция спирали XY, содержит только перемещения в плане XY
spiral_XY stepXY partLenth fullThickness = do
  cur_x <- sysVar 5001
  cur_y <- sysVar 5002

  xp0 <- newVar (0 :: Double)
  yp0 <- newVarE (-stepXY )
  xp1 <- newVarE (partLenth)
  yp1 <- newVarE (fullThickness)

  gwhile true $ do --Цикл обработки по XY (итерационный)
    frame [g 01, x xp1] {- переходим на подачу резанья по длине 1 -}
    frame [g 2, x $ xp1 + stepXY , y yp0, r  $ stepXY ] {- поворачиваем на обратный ход 2 -}
    gIf (cur_y < -(yp1 - stepXY) ) $ do --frame [g 01, x xp0]
                                        break
    frame [g 01, y $ -(yp1 - stepXY) ] {- движемся в  направлении толщины 3  -}
    frame [g 2, y $ - yp1, x xp1, r stepXY ] {- поворачиваем направо 4 -}
    gIf (cur_x < xp0 + stepXY) $ do --frame [g 01, y $  yp0 ]
                                    break
    frame [g 01, x $ xp0 + stepXY ] {- движемся в  направлении длины 5  -}
    frame [g 2, x xp0, y $ - (yp1 - stepXY), r stepXY ] {- поворачиваем направо 6 -}
    gIf (cur_y > yp0 - stepXY) $ do --frame [g 01, x $ xp1]
                                    break
    frame [g 01, y $ yp0 - stepXY] {- движемся в  направлении толщины 7  -}
    frame [g 02, r stepXY, x $ xp0 + stepXY, y yp0] {- поворот на следующий круг 8-}
    xp0 #= xp0 + stepXY
    yp0 #= yp0 - stepXY
    xp1 #= xp1 - stepXY
    yp1 #= yp1 - stepXY
    gIf (cur_x > xp1) $ do --frame [g 01, y $ - yp1]
                           break

-- функция подготовки и предварительного позиционирования
prepare_preposition_block nameTool rpoinXY cur_d fullHigh rpoinZ = do
  frame [g 00, x $ -(rpoinXY + cur_d), y $ -(rpoinXY + cur_d)]  --Позиционируемся наверху для безопасного опускания
  frame [g 43, h nameTool, z $ fullHigh + 150.0] -- коррекця по высоте инструмента + опускаемся до границы в 150мм по Z
  frame [m 03, s 8000] --Запускаем шпиндель по часовой с оборотами 8000
  frame [m 08] -- включаем эмульсию
  frame [g 01, z $ fullHigh + rpoinZ, f 10000] -- переходим в R-point Z с подачей в 10000

-- функция Завершения операции, отключение эмульсии, шпинделя, продувка заготовки
operation_end fullHigh spointZ = do
  frame [g 00, z $ fullHigh + spointZ] {- Поднимаемся в s-point  -}
  frame [m 05] {- отключаем шпиндель -}
  frame [m 09] {- отключаем эмульсию -}
  frame [g 00, z 250.0]
  frame [x 50.0, y 240.0]
  frame [m 07]  {- дуем воздухом -}
  frame [g 04, p 1500]  {- ждём 1.5 сек -}
  frame [m 09]  {- отключаем воздух -}

main = do
  putStrLn "%"
  putStrLn "O4001 (Gabarit AL  spiral 2 storoni)"
  putHCode hcode_prog2
  putStrLn "\n%"