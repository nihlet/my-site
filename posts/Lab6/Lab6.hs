module Lab6
  (lab6
  ) where

import Lab5(fir, discrets, signal)

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy             hiding (indices, tan)

import Data.Array
import Data.Complex
import Numeric.Transform.Fourier.FFT

-- Вспомогательные величины
fopt :: FileOptions
fopt = def{_fo_size=(600,200)}

rect :: FileOptions
rect = def{_fo_size=(400,400)}

-- Константы согласно заданию
fpass = 3500
attPass = 3 
fstop = 4000
attStop = 28

fd = 10000

-----------------------------------------------------
j = 0 :+ 1 -- мнимая единица

fromdB att = sqrt (10**(0.1 * att) - 1)

n = logBase (fstop / fpass) (fromdB attStop / fromdB attPass) -- ищем порядок фильтра


lab6 :: IO () 
lab6 = do
  plotSignal
  plotZeroPole


plotSignal =
  toFile fopt "posts/Lab6/signal.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal])


plotZeroPole =
  toFile rect "posts/Lab6/zeroPole.svg" $do
    setColors [opaque blue, opaque red]
    plot (line "" [circle]) 
    plot (points "" poles) where
      circle = [(sin x, cos x) | x <- [0, pi/180 .. 2*pi::Double]]
      poles = [(sin x, cos x) | x <- [0, pi/n .. 2*pi::Double]]
      





