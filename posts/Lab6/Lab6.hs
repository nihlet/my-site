module Lab6
  (lab6
  ) where

import Lab5(fir, discrets, signal)

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy             hiding (indices, tan)

import DSP.Filter.IIR.Design
import DSP.Filter.IIR.IIR

import Butterworth

-- Вспомогательные величины
fopt :: FileOptions
fopt = def{_fo_size=(600,200)}

rect :: FileOptions
rect = def{_fo_size=(400,400)}

-- -- Константы согласно заданию
-- fpass = 3500
-- attPass = 3 
-- fstop = 4000
-- attStop = 28

-- fd = 10000

-- MATLAB Example
fpass = 40
fstop = 150

attPass = 3
attStop = 60

fd = 1000

fp = fpass/(fd/2) 
fs = fstop/(fd/2) 


-----------------------------------------------------
-- fromdB att = sqrt (10**(0.1 * att) - 1)

fromdB att = 10**(0.1 * att)
epsilon att = sqrt (fromdB att - 1)

n = ceiling $ logBase (fs / fp) (epsilon attStop / epsilon attPass) -- ищем порядок фильтра

butterworth = butterworthLowpass (fp, fromdB (-attPass)) (fs, fromdB (-attStop))

lab6 :: IO () 
lab6 = do
  plotSignal
  -- plotZeroPole
  plotSignalFiltered


plotSignal =
  toFile fopt "posts/Lab6/signal.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal])

plotSignalFiltered =
  toFile fopt "posts/Lab6/signalFiltered.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal])where 
      signal' = iir_df1 butterworth signal


-- plotZeroPole =
--   toFile rect "posts/Lab6/zeroPole.svg" $do
--     setColors [opaque blue, opaque red]
--     plot (line "" [circle]) 
--     plot (points "" poles) where
--       circle = [(sin x, cos x) | x <- [0, pi/180 .. 2*pi::Double]]
--       poles = [(sin x, cos x) | x <- [0, pi/n .. 2*pi::Double]]
      

-----------------------------------------------------









