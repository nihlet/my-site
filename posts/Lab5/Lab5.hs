module Lab5
  (lab5
  ,fir
  ,discrets
  ,signal
  ) where

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy             hiding (indices)

import Data.Array
import Data.Complex
import Numeric.Transform.Fourier.FFT

-- Вспомогательные величины
fopt :: FileOptions
fopt = def{_fo_size=(600,200)}

-- Константы согласно заданию
a = 4 -- амплитуда сигнала

f0 = 380.0 -- частота следования импульсов
fv = 19000.0 -- верхняя частота сигнала
r = 0.92 -- параметр затухания резонатора
fc = 7000.0 -- частота настройки фильтра

-----------------------------------------------------

period = 1 / f0
fd = fv * 8

j = 0 :+ 1 -- мнимая единица

chirp :: Double -> Double
chirp t = a * cos(2 * pi * fv / (2 * period) * t' ^^ 2) where
    t' = t - period * fromIntegral (floor (t / period))

discrets = [0.0, 1/fd .. 2 * period]
signal = map chirp discrets

-- a, b список коэффициентов БИХ фильтра. Например фильтр 2 порядка: [a2,a1,a0] [b2,b1,b0]
-- xs отсчеты сигнала
iir :: Num a => [a] -> [a] -> [a] -> [a]
iir a b xs = helper n zeros where
  n = zeros ++ xs -- изначальный сигнал дополненный нулями
  zeros = replicate rank 0
  rank = max (length b) (length a) - 1
  helper [] _ = []  -- вспомогательная функция для прохода по всему сигналу
  helper ns mem = next : helper (tail ns) mem' where
    next = sum (zipWith (*) b ns) - sum (zipWith (*) a mem)
    mem' = next : init mem 

  
-- КИХ фильтр как частный случай БИХ фильтра
fir = iir []

lab5 :: IO () 
lab5 = do
  plotSignal
  plotFIR1
  plotFIR2
  plotIIR1
  plotIIR2
  plotTransferIIR2

-- черезпериодный вычитатель 1 порядка
fir1 = fir [-1, 1]

-- черезпериодный вычитатель 2 порядка
fir2 = fir [0.5, -1, 0.5]

-- рециркулятор
iir1 = iir [-r] [1]

-- Комплексный резонатор
iir2 = iir [-k] [1] where
  k = (r :+ 0) * exp (j * 2 * pi * ((fc / fd) :+ 0) )

plotSignal =
  toFile fopt "posts/Lab5/signal.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal])
      
plotFIR1 =
  toFile fopt "posts/Lab5/signal_fir1.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal']) where
      signal' = fir1 signal

plotFIR2 =
  toFile fopt "posts/Lab5/signal_fir2.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal']) where
      signal' = fir2 signal

plotIIR1 =
  toFile fopt "posts/Lab5/signal_iir1.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal']) where
      signal' = iir1 signal

plotIIR2 =
  toFile fopt "posts/Lab5/signal_iir2.svg" $do
    setColors [opaque blue, opaque red, opaque red]
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    plotLeft  (line "" [zip discrets signal'']) 
    plotRight (line "" [zip discrets $ map freq discrets]) 
    plotRight (points "" [(7000.0 * period / fv, 7000.0), (7000.0 * period / fv + period, 7000.0)]) -- todo 90% что здесь ошибка
    where
      signal' = iir2 $ map (:+ 0) signal
      signal'' = map realPart signal'
      freq t = fv / period * t' where
        t' = t - period * fromIntegral (floor (t / period))

plotTransferIIR2 = 
  toFile fopt "posts/Lab5/signal_iir2_transfer.svg" $do
    setColors [opaque blue]
    plot (line "" [zip freq (map transfer freq)]) where
      transfer f =1 / sqrt (1 + r^^2 - 2 * r * cos (2 * pi * (f - fc) / fd))
      freq = [0, fv/1000 .. fv]
      




