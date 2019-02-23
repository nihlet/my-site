module Lab6
  (lab6
  ) where

import Lab5(iir)

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy             hiding (indices, tan)
import DSP.Filter.IIR.IIR

import MathObj.Polynomial hiding (reverse)

import Data.Complex
import Data.Array

import Butterworth (butterworthIIR)

-- Вспомогательные величины
fopt :: FileOptions
fopt = def{_fo_size=(600,200)}

rect :: FileOptions
rect = def{_fo_size=(400,400)}

-- Константы согласно заданию
fpass = 3500 :: Double
attPass = 3 
fstop = 4000
attStop = 4

fd = 10000
-----------------------------------------------------

period = 1/50

a = 1 

fv = 5000

chirp :: Double -> Double
chirp t = a * cos(2 * pi * fv / (2 * period) * t' ^^ 2) where
    t' = t - period * fromIntegral (floor (t / period))

discrets = [0.0, 1/fd .. 2 * period]
signal = map chirp discrets

butterworth = iir (reverse a) (reverse b) where 
  (a,b) = butterworthIIR fd (fpass, fstop) (attPass, attStop)

lab6 :: IO () 
lab6 = do
  plotSignal
  -- plotZeroPole
  plotSignalFiltered
  plotTransfer


plotSignal =
  toFile fopt "posts/Lab6/signal.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal])

plotSignalFiltered = do
  print $ take 20 signal'
  print $ butterworthIIR fd (fpass, fstop) (attPass, attStop)
  toFile fopt "posts/Lab6/signalFiltered.svg" $do
    setColors [opaque black, opaque blue]
    -- plot (line "" [zip discrets signal])
    plot (line "" [zip discrets signal'])where 
      -- signal' = butterworth signal
      signal' = iir_df1 (bArr, aArr) signal
      (a, b) = butterworthIIR fd (fpass, fstop) (attPass, attStop)
      aArr = listArray (0, length a - 1) a
      bArr = listArray (0, length b - 1) b

trasfer omega = num / den where 
  (a, b) = butterworthIIR fd (fpass, fstop) (attPass, attStop)
  num = evaluate (fromCoeffs $ map (:+0) b) (exp (-(omega :+ 0) * j))
  den = evaluate (fromCoeffs $ map (:+0) a) (exp (-(omega :+ 0) * j))
  j = 0 :+ 1


plotTransfer =
  toFile fopt "posts/Lab6/transfer.svg" $do
    setColors [opaque blue]
    plot (line "" [zip freq $ map (logValue . magnitude . trasfer) freq]) where 
      freq = [0,1/180 .. pi]
      logValue x = 10 *logBase 10 x

-- plotZeroPole =
--   toFile rect "posts/Lab6/zeroPole.svg" $do
--     setColors [opaque blue, opaque red]
--     plot (line "" [circle]) 
--     plot (points "" poles) where
--       circle = [(sin x, cos x) | x <- [0, pi/180 .. 2*pi::Double]]
--       poles = [(sin x, cos x) | x <- [0, pi/n .. 2*pi::Double]]
      

-----------------------------------------------------









