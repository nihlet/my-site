module Lab6
  (lab6
  ) where

import Butterworth 
import Data.Array
import Data.Complex
import DSP.Filter.IIR.IIR
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy             hiding (indices, tan)
import Lab5(iir)
import MathObj.Polynomial hiding (reverse)


-- Вспомогательные величины
fopt :: FileOptions
fopt = def{_fo_size=(600,200)}

rect :: FileOptions
rect = def{_fo_size=(400,400)}

-- Константы согласно заданию
fpass = 3500 :: Double
attPass = 4 
fstop = 4000
attStop = 28

fd = 10000
-----------------------------------------------------

period = 1/20

fv = fd / 2

chirp :: Double -> Double
chirp t = a * cos(2 * pi * fv / (2 * period) * t' ^^ 2) where
    t' = t - period * fromIntegral (floor (t / period))
    a = 1 

discrets = [0.0, 1/fd .. 2 * period]
signal = map chirp discrets

logValue x = 10 * logBase 10 x

butterworth = iir a b where 
  (a,b) = butterworthIIR fd (fpass, fstop) (attPass, attStop)

lab6 :: IO () 
lab6 = do
  plotSignal
  plotZeroPole
  plotSignalFiltered
  plotTransfer


plotSignal =
  toFile fopt "posts/Lab6/signal.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal])

plotSignalFiltered = 
  toFile fopt "posts/Lab6/signalFiltered.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal'])where 
      signal' = butterworth signal

transfer bt omega = num / den where 
  (a, b) = bt
  num = evaluate (fromCoeffs $ map (:+0) b) (exp (-(omega :+ 0) * j))
  den = evaluate (fromCoeffs $ map (:+0) a) (exp (-(omega :+ 0) * j))
  j = 0 :+ 1


plotTransfer =
  toFile fopt "posts/Lab6/transfer.svg" $do
    setColors [opaque black, opaque blue, opaque red]
    layout_y_axis . laxis_generate .= scaledAxis def (-40, 0)
    plot (line "" [zip freq $ map (logValue . magnitude . transfer bt1) freq]) where 
      freq = [0, pi/180 .. pi]
      bt1 = butterworthIIR fd (fpass, fstop) (attPass, attStop)


plotZeroPole =
  toFile rect "posts/Lab6/zeroPole.svg" $do
    setColors [opaque blue, opaque red]
    plot (line "" [circle]) 
    plot (points "" poles) where
      circle = [(cos x, sin x) | x <- [0, pi/180 .. 2*pi::Double]]
      poles = [(cos x, sin x) | x <- xpoles]
      n = fromIntegral $ order fd (fpass, fstop) (attPass, attStop)
      xpoles = filter (<=(3/2 * pi)) $ filter (>=pi/2) [0, pi/8 .. 2 * pi::Double]