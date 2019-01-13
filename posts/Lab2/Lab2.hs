module Lab2
  (lab2) where

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy             hiding (indices)

import Data.Array
import Data.Complex
import Numeric.Transform.Fourier.FFT

-- Константы согласно заданию
f0 :: Double
f0 = 1400

k :: [Double]
k = [0.2, 1.6, 8.0]

m :: [Int]
m = [2, 5, 12]

n = 1024

periods :: Double
periods = 10

period :: Double
period = 1 / f0

function :: Double -> Double
function t = fromInteger $ (-1) ^ i
  where
    i = floor (t * f0 * 2)

lab2 :: IO () 
lab2 = do
  plotOriginal
  plotOriginalAmpSpectrum
  plotOriginalPhaseSpectrum
  plotUndersampling1
  plotUndersamplingAmpSpectrum1
  plotUndersamplingPhaseSpectrum1
  plotUndersampling2
  plotUndersamplingAmpSpectrum2
  plotUndersamplingPhaseSpectrum2
  plotOversampling
  plotOversamplingAmpSpectrum
  plotOversamplingPhaseSpectrum


ampSpectrum signal = map magnitude $ elems $ rfft $ listArray (0, n-1) signal where
  n = length signal

phaseSpectrum signal = map phase $ elems $ rfft $ listArray (0, n-1) signal where
    n = length signal

-- Original signal
plotOriginal =
  toFile def "posts/Lab2/Original_signal.svg" $do
    layout_title .= "Original signal"
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      fd = n / (periods * period)

plotOriginalAmpSpectrum = 
  toFile def "posts/Lab2/Original_spectrum.svg" $do
    layout_title .= "Original spectrum"
    setColors [opaque blue]
    plot (line "" [zip discrets spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = ampSpectrum $ map function discrets
      fd = n / (periods * period)

plotOriginalPhaseSpectrum = 
  toFile def "posts/Lab2/Original_phase_spectrum.svg" $do
    layout_title .= "Original phase spectrum"
    setColors [opaque blue]
    plot (line "" [zip discrets spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = phaseSpectrum $ map function discrets
      fd = n / (periods * period)

-- Undersampling signal
plotUndersampling1 =
  toFile def "posts/Lab2/undersamling_signal1.svg" $do
    layout_title .= "Undersampling signal, k = 0.2"
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      fd = f0 * head k

plotUndersamplingAmpSpectrum1 = 
  toFile def "posts/Lab2/undersamling_spectrum1.svg" $do
    layout_title .= "Undersampling specrtum, k = 0.2"
    setColors [opaque blue]
    plot (line "" [zip discrets spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = ampSpectrum $ map function discrets
      fd = f0 * head k

plotUndersamplingPhaseSpectrum1 = 
  toFile def "posts/Lab2/undersamling_phase_spectrum1.svg" $do
    layout_title .= "Undersampling phase specrtum, k = 0.2"
    setColors [opaque blue]
    plot (line "" [zip discrets spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = phaseSpectrum $ map function discrets
      fd = f0 * head k

-- Undersampling signal 2
plotUndersampling2 =
  toFile def "posts/Lab2/undersamling_signal2.svg" $do
    layout_title .= "Undersampling signal, k = 1.6"
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      fd = f0 * k!!1

plotUndersamplingAmpSpectrum2 = 
  toFile def "posts/Lab2/undersamling_spectrum2.svg" $do
    layout_title .= "Undersampling specrtum, k = 1.6"
    setColors [opaque blue]
    plot (line "" [zip discrets spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = ampSpectrum $ map function discrets
      fd = f0 * k!!1

plotUndersamplingPhaseSpectrum2 = 
  toFile def "posts/Lab2/undersamling_phase_spectrum2.svg" $do
    layout_title .= "Undersampling phase specrtum, k = 1.6"
    setColors [opaque blue]
    plot (line "" [zip discrets spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = phaseSpectrum $ map function discrets
      fd = f0 * k!!1

-- Oversampling signal
plotOversampling =
  toFile def "posts/Lab2/oversamling_signal2.svg" $do
    layout_title .= "Oversampling signal, k = 1.6"
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      fd = f0 * k!!2

plotOversamplingAmpSpectrum = 
  toFile def "posts/Lab2/oversamling_spectrum2.svg" $do
    layout_title .= "Oversampling specrtum, k = 1.6"
    setColors [opaque blue]
    plot (line "" [zip discrets spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = ampSpectrum $ map function discrets
      fd = f0 * k!!2

plotOversamplingPhaseSpectrum = 
  toFile def "posts/Lab2/oversamling_phase_spectrum2.svg" $do
    layout_title .= "Oversampling phase specrtum, k = 1.6"
    setColors [opaque blue]
    plot (line "" [zip discrets spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = phaseSpectrum $ map function discrets
      fd = f0 * k!!2
    
