module Lab5
  (lab5) where

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy             hiding (indices)

import Data.Array
import Data.Complex
import Numeric.Transform.Fourier.FFT

-- Вспомогательные величины
fopt :: FileOptions
fopt = def{_fo_size=(600,200)}

-- Константы согласно заданию
tau = 1.0

a = 4 -- амплитуда сигнала

f0 = 380.0 -- частота следования импульсов
fv = 19000.0 -- верхняя частота сигнала
r = 0.92 -- параметр затухания резонатора
fc = 7000.0 -- частота настройки фильтра

-----------------------------------------------------

period = 1 / f0
fd = fv * 8

chirp :: Double -> Double
chirp t = a * cos(2 * pi * fv / (2 * period) * t ^^ 2)

-- a, b список коэффициентов БИХ фильтра. Например фильтр 2 порядка: [a2,a1,a0] [b2,b1,b0]
-- xs отсчеты сигнала
iir :: [Double] -> [Double] -> [Double] -> [Double]
iir a b xs = helper n zeros where
  n = zeros ++ xs -- изначальный сигнал дополненный нулями
  zeros = replicate rank 0.0
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


plotSignal =
  toFile fopt "posts/Lab5/signal.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      signal = map chirp discrets
      discrets = [0.0, 1/fd .. 2 * period]

plotFIR1 =
  toFile fopt "posts/Lab5/signal_fir1.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      signal = map chirp discrets
      discrets = [0.0, 1/fd .. 2 * period]

