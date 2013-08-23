> import Expr
> import GCode

Программа
---------

Рассмотрим простенькую программу с goto, метками, if, переменными, выражениями и фреймами и варианты описания на edsl

  start: #101 = [ #101 + 42 ]
  G1 X#101 Y#100 Z20
  #100 = [ #100 - 5 ]
  IF [ #100 GT 0 ] THEN GOTO start
  end: M100

Первый вариант
--------------

> prog1 :: GCode ()
> prog1 = do

>   label "start"

Программа начинается с метки start. Хорошее начало.

>   var101 <- nameCell 101
>   var100 <- nameCell 100

Далее привязываем имя var101 к ячеке #101 и var100 к #100.
  Просто отлично, что теперь у нас есть имена!

>   var101 #= (gRead var101 + 42)

gRead наглядно дает понять императивную природу ячеек с памятью, но возможно чрезмерное его использование
  сильно утяжелит код, который и так по природе императивен.

>   frame [G 1, X (gRead var101), Y (gRead var100), Z 20]

frame группирует инструкции в одну транзакцию, которая выполняется станком. Он прекрасен, когда инструкци много.

>   var100 #= (gRead var100 - 5)

>   gIf (gRead var100 #> 0)
>     (goto "start")
>     (return ())

- В gIf левая и правая ветка группируется скобочками, что не очень удобно

>   label "end"
>   frame [M 100]

А вот пример того, что frame может задолбать на одиночных инструкциях.
Основные недостатки - это чрезмерное использование gRead и frame для одиночных команд

Второй вариант с полиморфными fX, fY, fZ, fG, fM:
------------------------------------------------

> prog2 :: GCode ()
> prog2 = do

>   label "start"
>   var101 <- nameCell 101
>   var100 <- nameCell 100
>   var101 #= (gRead var101 + 42)
>   frame [fG 1, fX (gRead var101), fY (gRead var100), fZ 20]

frame группирует инструкции в одну транзакцию, которая выполняется станком. Он прекрасен, когда инструкци много.

>   var100 #= (gRead var100 - 5)
>   gIf (gRead var100 #> 0)
>     (goto "start")
>     (return ())
>   label "end"

fM можно вставлять без frame

>   fM 100
