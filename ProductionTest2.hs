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
  terrace_high <- newVar 45.8 #"visota ustupa"
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
--только функции SDL
  
  --начало самой программы
  --пошла сама программа
  frame [g 91, g 28, z 0] -- поднимаемся в референтную позицию по Z
  frame [g 58, g 90, g 21, g 17, g 23, g 40, g 49, g 80] --строка безопасности
  frame [m 06, t nameTool] -- Меняем инструмент на указанный в настройках
  

  frame [g 43, h nameTool, z $ fullHigh + spointZ ] {- активируем коррекцию по высоте заданого инструмент и опускаемся в S-point -}
  frame [g 01, z $ fullHigh, f feedCut] {-Опускаемся на уровень Z0 заготовки-}

-- черновая обработка уступов 2х деталей
-- инициализация параметров обработки
  nameTool <- newVar (1 :: Int) # "instrument"
  stepZ <- newVar (1 :: Double) # "shag po Z"
  feedCut <- newVar 2400 # "vrezaie"
  feedPlunge <- newVar 800 # "rezania"
  change_tool_start nameTool fullHigh 8000
  first_part_preposition_XY rpoinXY cur_d -- отходим в сторону для удовлетворения g41/42
  y_dim_center <- newVarE $ partThickness - terrace_width --задаём толщину оставшегося материала от центра в обоих направлениях для каждой детали
  double_part_2X_cycle y_dim_center terrace_high partThickness partHigh partLength allowance cur_z
  first_part_preposition_XY rpoinXY cur_d -- отходим в сторону для финального захода
  --чистовой проход первым инструментом
  frame [g 01, z $ parallelHigh + partHigh - terrace_high] -- позиционируеся по высоте
  exterior_2_parts_X y_dim_center partLength partThickness allowance --проходим последний круг
  --переходим к чистовому фрезерованию стенки
  iteration_end fullHigh spointZ
  feedCut <- newVar 2400 # "vrezaie"
  feedPlunge <- newVar 800 # "rezania"
  change_tool_start 15 fullHigh 10500
  first_part_preposition_XY rpoinXY cur_d
  frame [g 01, z $ parallelHigh + partHigh - terrace_high] -- позиционируеся по высоте
  exterior_2_parts_X y_dim_center partLenth partThickness 0 15 --чистовой проход 15м инструментом
  iteration_end fullHigh spointZ
--переходим к фрезерованию внутреннего скоса 30 градусов
  change_tool_start 13 fullHigh 12500
  stepZ #= 0.1
  first_part_preposition_XY rpoinXY cur_d --подходим к первой детали
  tan_k <- newVarE $ tan (180 - angle_near_wall - 90) -- вычисляем тангенс нужного нам угла
  -- пошёл цикл обрабоки скоса 30 град
  while (cur_z > parallelHigh + fullHigh - terrace_high - near_wall_high) $ do
    z $ cur_z - stepZ
    y_dim_center #= cur_y - tan_k * stepZ
    -- вызываем функцию прохода по внутреннему краю
    interior_2_parts_X y_dim_center partLength partThickness nameTool cur_d 0
    z_count #= z_count + 1.0
  operation_end
  change_tool_start 16 fullHigh 8000
  first_part_preposition_XY rpoinXY cur_d --подходим к первой детали
  z $ parallelHigh + terrace_high + 
  exterior_2_parts_X y_dim_center partLength partThickness nameTool 0


--объявленные функции
--цикл по выборке 2х уступов в направлении X до определённой высоты
double_part_2X_cycle  y_dim_center terrace_high partThickness partHigh partLength allowance cur_z = do --цикл обработки по Z
  cur_cycle_steps <- newVarE fix $ (partHigh - terrace_high) / stepZ -- количество целых шагов цикла
  z_count <- newVar (0 : int) --инициализируем счётчик
  while  (cur_cycle_steps < z_count) $ do -- запускаем цикл
    frame [g 01,z $ cur_z - stepZ] -- опускаемся на шаг
    exterior_2_parts_X y_dim_center partLength partThickness allowance -- вызываем движение по кругу
    z_count #= z_count + 1


--проход по 2м деталям с внутренней стороны
interior_2_parts_X y_dim_center partLength partThickness nameTool cur_d allowance = do
  frame [g 42, d nameTool, y $ y_dim_center + allowance] -- выходим на первую деталь
  frame [g 01, x 0, f feedPlunge] -- врезаемся
  frame [x partLength + rpoinXY + cur_d * 3,f feedCut] -- режем первую
  frame [g 00, y $ partThickness + y_dim_center + allowance] --переходим на вторую деталь
  frame [g 01, x $  partLength - rpoinXY, f feedPlunge]
  frame [x $ - (rpoinXY + cur_d * 3)]
  frame [y $ y_dim_center + allowance] -- выходим на первую деталь
  g 40

--проход по 2м деталям с внешней стороны
exterior_2_parts_X y_dim_center partLength partThickness nameTool allowance = do
  frame [g 41, d nameTool, y $ - (partThickness - y_dim_center - allowance )] --выходим на первую деталь
  frame [g 01, x 0, f feedPlunge] --врезаемся в первую деталь
  farme [x $ partLength + rpoinXY, f feedCut] --осуществляем резание на всю длинну
  frame [g 00,y $ - (partThickness + y_dim_center + allowance )] --выходим на вторую деталь
  frame [g 01, x partLenth, f feedPlunge] -- врезаемся во вторую деталь
  frame [x - rpoinXY, f feedCut] -- осуществляем резанье на всю длинну
  frame [y $ - (partThickness - y_dim_center - allowance )] --выходим опять на первую деталь
  g 40

-- смена инструмента
change_tool_start nameTool fullHigh s_spindel = do
  frame [m 06, t nameTool]
  frame [g 00, g 43, h nameTool, z $ fullHigh + 150.0 ]
  m 08
  frame [m 03, s s_spindel]
  
-- завершение операции
iteration_end fullHigh spointZ = do
  z $ fullHigh + spointZ {- Поднимаемся в s-point  -}
  frame [m 05] {- отключаем шпиндель -}
  frame [m 09] {- отключаем эмульсию -}

operation_end fullHigh spointZ = do
  z $ fullHigh + spointZ {- Поднимаемся в s-point  -}
  frame [m 05] {- отключаем шпиндель -}
  frame [m 09] {- отключаем эмульсию -}
  frame [g 00, x 50.0, y 240.0, z 250.0]
  frame [m 07]  {- дуем воздухом -}
  frame [g 04, p 1500]  {- ждём 1.5 сек -}
  frame [m 09]  {- отключаем воздух -}

--препозиция XY для первой детали
first_part_preposition_XY rpoinXY cur_d = do
  frame [g 00, x $ -(rpoinXY + cur_d), y $ -(rpoinXY + cur_d * 3)]

--препозиция XY для второй детали
second_part_prepositon_XY partLent rpoinX = do
  frame [g 00, x $ partLength + rpoinXY + cur_d, y $ - (partThickness * 3 + cur_d + rpoinXY)]

main = do
  putStrLn "%"
  putStrLn "O4201 (culaga universal Exprof)"
  putHCode hcode_prog2
  putStrLn "\n%"