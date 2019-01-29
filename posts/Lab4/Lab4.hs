module Lab4
  (lab4) where

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

a = 0.8
l = 4

f0 = 1300.6
fa = 11.3
fd = 4096

n = fd * tau

function :: Double -> Double
function t = (1 + a * cos(2* pi * fa * t)) * cos(2 * pi * f0 *t)

lab4 :: IO () 
lab4 = do
  plotSignal
  plotSpectrum

ampSpectrum signal = map magnitude $ elems $ rfft $ listArray (0, n-1) signal where
  n = length signal

phaseSpectrum signal = map phase $ elems $ rfft $ listArray (0, n-1) signal where
    n = length signal


plotSignal =
  toFile fopt "posts/Lab4/signal.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      discrets = [0.0, 1.0 /fd .. 1]
      signal = map function discrets

plotSpectrum =
  toFile fopt "posts/Lab4/spectrum.svg" $do
    -- layout_x_axis . laxis_generate .= scaledAxis def (0,n)
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = init [0.0, 1.0 /fd .. 1]
      spectrum = map (* (2 / n)) $ ampSpectrum $ map function discrets
      harmonics = [0.0, 1.0 .. n]

-- plotPhaseSpectrum =
--   toFile fopt "posts/Lab4/signal.svg" $do
--     setColors [opaque blue]
--     plot (line "" [zip discrets signal]) where
--       discrets = [0.0, 1.0 /fd .. 1]
--       signal = map function discrets

      