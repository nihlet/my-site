module Lab5
  (lab5
  ,iir
  -- ,discrets
  -- ,signal
  ) where

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy             hiding (indices)

import Data.Array
import MathObj.Polynomial 
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

-- a, b список коэффициентов БИХ фильтра. Например фильтр 2 порядка: [a0,a1,a2] [b0,b1,b2]
-- xs отсчеты сигнала
iir :: Num a => [a] -> [a] -> [a] -> [a]
iir a b xs = helper xs zeros zeros where
  zeros = replicate rank 0
  rank = max (length b) (length a)  -- - 1
  helper [] _ _ = []  -- вспомогательная функция для прохода по всему сигналу
  helper ns x_mem y_mem = next : helper (tail ns) x_mem' y_mem' where
    next = sum (zipWith (*) b x_mem') - sum (zipWith (*) a $ 0:y_mem)
    y_mem' = next : init y_mem 
    x_mem' = head ns : init x_mem

  
-- КИХ фильтр как частный случай БИХ фильтра
fir = iir []

lab5 :: IO () 
lab5 = do
  plotSignal
  plotFIR1
  plotFIR2
  plotIIR1
  plotIIR2
  plotIIR2Slow
  plotTransferIIR2

-- черезпериодный вычитатель 1 порядка
fir1 = fir [-1, 1]

-- черезпериодный вычитатель 2 порядка
fir2 = fir [0.5, -1, 0.5]

-- рециркулятор
iir1 = iir [1, -r] [1]

-- Комплексный резонатор
iir2 = iir [0,-k] [1] where
  k = (r :+ 0) * exp (j * pi * ((fc / (fd/2) ) :+ 0) )

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
    plotRight (points "" [(fc * period / fv, fc), (fc * period / fv + period, fc)]) where
      signal' = iir2 $ map (:+ 0) signal
      signal'' = map realPart signal'
      freq t = fv / period * t' where
        t' = t - period * fromIntegral (floor (t / period))


plotIIR2Slow =
  toFile fopt "posts/Lab5/signal_iir2_slow.svg" $do
    setColors [opaque blue, opaque red, opaque red]
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    plotLeft  (line "" [zip discrets signal'']) 
    plotRight (line "" [zip discrets $ map freq discrets]) 
    plotRight (points "" [(fc * period / fv, fc), (fc * period / fv + period, fc)]) where
      signal' = iir2 $ map (:+ 0) signal
      signal'' = map realPart signal'
      freq t = fv / period * t' where
        t' = t - period * fromIntegral (floor (t / period))

      -- делаем период сигнала больше, для того чтобы снизить скорость изменения частоты и переопределяем глобальные функции
      period  = 20 / f0 
      signal = map chirp discrets
      discrets = [0.0, 1/fd .. 2 * period]   

      chirp :: Double -> Double
      chirp t = a * cos(2 * pi * fv / (2 * period) * t' ^^ 2) where
        t' = t - period * fromIntegral (floor (t / period))

      

plotTransferIIR2 = 
  toFile fopt "posts/Lab5/signal_iir2_transfer.svg" $do
    setColors [opaque blue]
    plot (line "" [zip freq (map transfer freq)]) where
      transfer f =1 / sqrt (1 + r^^2 - 2 * r * cos (2 * pi * (f - fc) / (fd/2) ))
      freq = [0, fv/1000 .. fv]
      

-- logValue x = 10 * logBase 10 x

-- transfer (a,b) omega = num / den where 
--   num = evaluate (fromCoeffs $ map (:+0) b) (exp (-(omega :+ 0) * j))
--   den = evaluate (fromCoeffs a) (exp (-(omega :+ 0) * j))
--   j = 0 :+ 1


-- plotTransferIIR2 = 
--   toFile fopt "posts/Lab5/signal_iir2_transfer.svg" $do
--     setColors [opaque black, opaque blue, opaque red]
--     -- layout_y_axis . laxis_generate .= scaledAxis def (-40, 0)
--     plot (line "" [zip freq $ map (logValue . magnitude . transfer irr2) freq]) where 
--       freq = [0, pi/180 .. pi :: Double]
--       irr2 = ([0,-k], [1]) where
--         k = (r :+ 0) * exp (j * ((fc/(fd/2) * pi):+0))




