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
  spointZ <- newVar (15.0 :: Double) # "R-pointZ"
  comment "System variables"
  fastLinear <- newVar (1 :: Int) # "uskorenoe dvijenie: medlenno 1, bitsro 0"
  fullThickness <- newVar (partThickness * numberOfparts : Double)
  countZ <- newVar (0 :: Int)
  xp0 <- newVar (0)
  yp0 <- newVar (-cur_d )
  xp1 <- newVar (partLenth)
  yp1 <- newVar (fullThickness)
  cur_x <- sysVar 5001
  cur_y <- sysVar 5002
  cur_z <- sysVar 5003
  cur_d <- newVarE (25.0 :: Double)
  numStepsZ <- newVarE $ fix $ (partHigh + h_oversize) / stepZ  {- Целых шагов по Z Округлить до целых вниз #-}
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
  while (fi numStepsZ / 2 > fi countZ) $ do {- Цикл обработки по Z -}

    frame [g 02, r cur_d + rpoinXY, x 0, y 0, f feedPlunge] {- врезаемся в заготовку на радиус инструмента 8-}
    frame [g 01, x xp0, f feedCut]
    while (xp1 > cur_d) $ do {- Цикл обработки по XY -}

      frame [g 01, x (partLenth - stepXY) , f  feedCut] {- переходим на подачу резанья 1 -}
      frame [g 2, x $ -(rpoinXY + cur_d) ] {- поворачиваем на обратный ход 2 -}
      frame [y $ -stepXY ] {- движемся в  направлении толщины 3  -}
      frame [g 2, x $ -(rpoinXY + cur_d) ] {- поворачиваем направо 4 -}
      frame [y $ -stepXY ] {- движемся в  направлении толщины 5  -}
      frame [g 2, x $ -(rpoinXY + cur_d) ] {- поворачиваем направо 6 -}
      frame [y $ -stepXY ] {- движемся в  направлении толщины 7  -}
       frame [g 02, r stepXY, x stepXY, y stepXY, f feedPlunge] {- поворот на следующий круг 8-}
      countXY #= countXY + 1

    frame [y $ (rpoinXY + cur_d)] {-  возвращаемся в начало по цикла XY -}
    frame [z $ cur_z - stepZ] {- Опускаемся на шаг по Z  -}
    countZ #= countZ + 1
    countXY#=0

  z $ fullHigh + spointZ {- Поднимаемся в s-point  -}
  frame [m 05] {- отключаем шпиндель -}
  frame [m 09] {- отключаем эмульсию -}
  frame [g fastLinear, x 50.0, y 240.0, z 250.0, f psevdoFast]
  frame [m 07]  {- дуем воздухом -}
  frame [g 04, p 1500]  {- ждём 1.5 сек -}
  frame [m 09]  {- отключаем воздух -}

-- FIXME
--  frame [#3006=1] # "Perevernut zagotovki cherz Y" {- Останавливаемся и выводим сообщение -}



main = do
  putStrLn "\n***** Production: \n\n"
  gcodeGen hcode_prog2
