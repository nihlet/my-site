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
  plotSignalZoomed
  plotSpectrum
  plotSignalDecimated
  plotSpectrumDecimated
  plotSpectrumBetterResolution
  plotPrepForInterpSignal
  plotPrepForInterpSpectrum
  plotPrepForInterpSpectrum2
  plotSignalInterpolated

ampSpectrum signal = map magnitude $ elems $ rfft $ listArray (0, n-1) signal where
  n = length signal

phaseSpectrum signal = map phase $ elems $ rfft $ listArray (0, n-1) signal where
  n = length signal

signalFromHarmonics k signal = map ((*2) . realPart) $ elems $ ifft harmonics where
  spectrum = rfft $ listArray (0, n-1) signal
  harmonics = listArray (0, n-1) $ take k (elems spectrum) ++ replicate (n - k) (0.0 :+ 0.0)
  n = length signal

discrets = [0.0, 1.0 /fd .. tau]

plotSignal =
  toFile fopt "posts/Lab4/signal.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      signal = map function discrets

plotSignalZoomed =
  toFile fopt "posts/Lab4/signalZoomed.svg" $do
    layout_x_axis . laxis_generate .= scaledAxis def (0,60/fd)
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      signal = map function discrets

plotSpectrum =
  toFile fopt "posts/Lab4/spectrum.svg" $do
    layout_x_axis . laxis_generate .= scaledAxis def (0.9*f0,1.1*f0)
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = init [0.0, 1.0 /fd .. tau]
      spectrum = map (* (2 / n)) $ ampSpectrum $ map function discrets
      harmonics = [0.0, 1.0/tau .. n]

plotSignalDecimated =
  toFile fopt "posts/Lab4/signalDecimated.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      signal = map function discrets
      fd' = fd / 16.0
      discrets = init [0.0, 1.0 /fd' .. tau]

plotSpectrumDecimated =
  toFile fopt "posts/Lab4/spectrumDecimated.svg" $do
    layout_x_axis . laxis_generate .= scaledAxis def (0,8*fa)
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      fd' = fd / 16.0
      discrets = init [0.0, 1.0 /fd' .. tau]
      spectrum = map (* (2 / n)) $ ampSpectrum $ map function discrets
      harmonics = [0.0, 1.0/tau .. n]

plotSpectrumBetterResolution =
  toFile fopt "posts/Lab4/spectrumBetterResolution.svg" $do
    layout_x_axis . laxis_generate .= scaledAxis def (0.9*f0,1.1*f0)
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = init [0.0, 1.0 / fd .. tau]
      spectrum = map (* (2 / n)) $ ampSpectrum signal
      harmonics = [0.0, 1/(4.0 * tau) .. 4 * n]
      signal = map function discrets ++ replicate (3 * floor n) 0.0


plotPrepForInterpSignal =
  toFile fopt "posts/Lab4/prepForInterpSignal.svg" $do
    layout_x_axis . laxis_generate .= scaledAxis def (0,60/fd)
    setColors [opaque blue]
    plot (line "" [zip discrets' signal']) where
      signal = map function discrets
      discrets' = [0.0, 1.0/(fd * fromIntegral l) .. tau] 
      signal' = concatMap (\x -> x : replicate (l-1) 0.0) signal

plotPrepForInterpSpectrum =
  toFile fopt "posts/Lab4/prepForInterpSpectrum.svg" $do
    layout_x_axis . laxis_generate .= scaledAxis def (0,2*f0)
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      signal = map function discrets
      discrets' = [0.0, 1.0/(fd * fromIntegral l) .. tau] 
      signal' = concatMap (\x -> x : replicate (l-1) 0.0) signal
      spectrum = map (* (2 / n)) $ ampSpectrum signal'
      harmonics = [0.0, 1/tau .. fromIntegral l * n]

plotPrepForInterpSpectrum2 =
  toFile fopt "posts/Lab4/prepForInterpSpectrum2.svg" $do
    layout_x_axis . laxis_generate .= scaledAxis def (0,2*f0)
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum']) where
      signal = map function discrets
      discrets' = [0.0, 1.0/(fd * fromIntegral l) .. tau] 
      signal' = concatMap (\x -> x : replicate (l-1) 0.0) signal
      spectrum = map (* (2 / n)) $ ampSpectrum signal'
      spectrum' = take k spectrum ++ replicate (length spectrum - k) 0.0
      k = floor (1.1*f0 / (1/tau))
      harmonics = [0.0, 1/tau .. fromIntegral l * n]

plotSignalInterpolated =
  toFile fopt "posts/Lab4/signalInterpolated.svg" $do
    layout_x_axis . laxis_generate .= scaledAxis def (0,60.0/fd)
    setColors [opaque blue]
    plot (line "" [zip discrets' signal'']) where
      signal = map function discrets
      discrets' = [0.0, 1.0/(fd * fromIntegral l) .. tau] 
      signal' = concatMap (\x -> x : replicate (l-1) 0.0) signal
      spectrum = elems $ rfft $ listArray (0, n-1) signal'
      n = length signal'
      k = floor (1.5*f0 / (1/tau))
      signal'' = map ((*2) . realPart) $ elems $ ifft $ listArray (0,n-1) $ take k spectrum ++ replicate (n-k) 0.0