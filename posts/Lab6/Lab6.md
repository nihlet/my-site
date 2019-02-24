---
title: Расчет ФНЧ Баттерворта и реализация цифрового фильтра (задача 6)
author: Воронин Андрей
date: 2019-02-24
---

4 вариант. 

# Дано:
Ослабление \\(> 28\\) дБ при \\(f=4000\\) Гц и \\(< 4\\) дБ при \\(f=3500\\) Гц;

Частота следования импульсов \\(f_0=380\\)

# Задание:
Найти порядок фильтра, описать нуль-полюсную диаграмму, записать передаточную 
функцию, построить АЧХ фильтра (частота дискретизации 10000 Гц).
Составить алгоритм цифрового БИХ-фильтра. Реализовать его в программном коде.
Подать на вход действительный ЛЧМ-сигнал и представить результат прохождения сигнала через ЦФ.

# Этапы решения задания

Относитенльная частота пропускания и частота остановки цифрового фильтра находятся из выражения:

\\[ \\omega = \\frac{f} {(f_d / 2)} \\cdot \\pi\\].

Поскольку переход от аналогового фильтра к цифровому осуществляется через 
билинейное преобразование, которое искажает шкалу частот, то на первом шаге 
необходимо учесть это искажение для того чтобы сформулировать требования к 
коридору АЧХ аналогового фильтра таким образом, чтобы на пятом шаге при 
билинейном преобразовании получить именно тот фильтр что нам нужен. Искажение 
шкалы частот при билинейном преобразовании происходит согласно выражению:

\\[ \\Omega = 2\\cdot f_d \\tan (\\omega/2) \\].

## Порядок цифрового фильтра

Порядок цифрового фильтра определяется по формуле аналогового фильтра:

\\[ N = \\frac{\\log(\\epsilon_s/\\epsilon_p)}{\\log(\\Omega_s/\\Omega_p)} \\],

где \\(\\epsilon_p = \\sqrt{10^{R_p/10}-1}\\), \\(\\epsilon_s = \\sqrt{10^{R_s/10}-1}\\).

Таким образом, порядок фильтра из имеющихся данных можно найти при помощи следующией функции:
``` Haskell
order fd (fp, fs) (attPass, attStop) = ceiling $ logBase (wsWrapped / wpWrapped) (epsilon attStop / epsilon attPass)
 where
  wpWrapped = 2 * fd * tan(wp/2) 
  wsWrapped = 2 * fd * tan(ws/2) 
  wp = fp/(fd/2) * pi
  ws = fs/(fd/2) * pi

fromdB att = 10**(0.1 * att)
```
Где отношение логорифмов приведено к логорифму по основанию.



## Нуль-полюсная диаграмма

![Рис. 1: Нуль-польюсная диаграмма](zeroPole.svg)


## Аналоговая передаточная функция

``` Haskell
analogTransferFunction :: (AF.C a, AZ.C a, RealFrac a, Floating a) =>
  (a, a) -> (a, a) -> Number.Ratio.T (MathObj.Polynomial.T a)
analogTransferFunction (wpWrapped, wsWrapped) (attPass, attStop) = const 1 % polynome where
  polynome = const (epsilon attPass) * p1^r * foldl (*) (const 1) (map p2 [1..l])
  p1 = fromCoeffs [alpha, 1]
  p2 n' = fromCoeffs [alpha^^2, 2 * alpha * sin(thetta n'), 1]
  thetta n' = (2 * fromIntegral n' - 1) / (2 * fromIntegral n) * pi
  alpha = epsilon attPass ** (-1 / fromIntegral n)
  (l,r) = divMod n 2
  n = orderWrapped (wpWrapped, wsWrapped) (attPass, attStop)
```


## Передаточная функция в z-плоскости

Переход к передаточной фунции в z-плоскости получается путем двух
последовательных подстановок при помощи функции `substitution`
 в аналоговую передаточную функцию:

- преобразование шкалы частот
- z-преобразование

``` Haskell
digitalTransferFunction :: (AF.C a, AZ.C a, RealFrac a, Floating a) =>
     a -> (a, a) -> (a, a) -> Number.Ratio.T (MathObj.Polynomial.T a)
digitalTransferFunction fd (wp, ws) (attPass, attStop) = substitution sub anTransfer' where
  sub = bilinear fd
  anTransfer = analogTransferFunction (wpWrapped, wsWrapped) (attPass, attStop)
  anTransfer' = substitution (s_omega :% const 1) anTransfer
  s_omega = fromCoeffs [0, 1 / wpWrapped]
  wpWrapped = 2 * fd * tan(wp/2) 
  wsWrapped = 2 * fd * tan(ws/2) 

bilinear:: (Num a) => a -> Number.Ratio.T (MathObj.Polynomial.T a) 
bilinear fd = (const (2 * fd) * fromCoeffs [1, -1]) :% fromCoeffs [1, 1]
```

Функция `substitution` определена как:
``` Haskell
-- подстановка дробь из полиномов в дробь из полиномов
substitution :: (AF.C a, AZ.C a, Num a, Fractional a) => 
  Number.Ratio.T (MathObj.Polynomial.T a) -> 
  Number.Ratio.T (MathObj.Polynomial.T a) -> Number.Ratio.T (MathObj.Polynomial.T a)
substitution (snum :% sden) (num :% den) = num'' :% den'' where
  maxExp = max (length (coeffs num) - 1) (length (coeffs den) - 1)
  num' = step3 $ step2 sden maxExp $ step1 snum num
  den' = step3 $ step2 sden maxExp $ step1 snum den
  num'' = const k * num' -- нормировка полиномов по 0 степени знаменателя
  den'' = const k * den'
  k = 1 / head (coeffs den')

step1 :: (Num a) =>  MathObj.Polynomial.T a -> MathObj.Polynomial.T a -> [MathObj.Polynomial.T a]
step1 subst poly = step1' 0 c where
  c = coeffs poly 
  step1' _ []     = []
  step1' n (x:xs) = (subst^n * const x) : step1' (n+1) xs

step2 :: (Num a, Integral b) => MathObj.Polynomial.T a -> b -> [MathObj.Polynomial.T a] -> [MathObj.Polynomial.T a]
step2 _ _ []     = []
step2 den n (x:xs) = x * den^n : step2 den (n-1) xs

step3:: (Num a) => [MathObj.Polynomial.T a] -> MathObj.Polynomial.T a
step3 = foldl (+) (const 0) 
``` 

Удобная интерфейсная функция для создация цифрового фильтра Баттерворта определена как:
``` Haskell
butterworthIIR :: (AF.C a, AZ.C a, RealFrac a, Floating a) =>
    a -> (a, a) -> (a, a) -> ([a], [a])
butterworthIIR fd (fp, fs) (attPass, attStop)  = (a,b) where
  b = coeffs $ numerator tf
  a = coeffs $ denominator tf  
  tf = digitalTransferFunction fd (wp, ws) (attPass, attStop)
  wp = fp/(fd/2) * pi
  ws = fs/(fd/2) * pi
```

Результат вызова функции с заданными параметрами дает нам фильтр 7 порядка:
``` Haskell
> butterworthIIR 10000 (3500,4000) (4,28)

([1.0,2.6754547177527477,3.7351846414329093,3.1471218198282607,
1.7090587847979137,0.5855567013649557,0.11645759969899837,1.0280589401484677e-2],
[0.10139933479904117,0.7097953435932882,2.1293860307798647,3.548976717966441,
3.548976717966441,2.1293860307798647,0.7097953435932882,0.10139933479904117])
```

## АЧХ фильтра

АЧХ фильтра находится путем подстановки \\(z^{-1} \\rightarrow e^{j \\omega}\\)

![Рис. 2: АЧХ цифрового фильтра](transfer.svg)


## Сигнал и его фильтрация

![Рис. 3: ЛЧМ сигнал](signal.svg)

Фильтрация производится функцией определенной в [задании 5](/posts/Lab5/Lab5.html).

![Рис. 4: Сигнал после фильтрации](signalFiltered.svg)


