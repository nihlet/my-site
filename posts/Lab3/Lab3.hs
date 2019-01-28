module Lab3
  (lab3) where

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy             hiding (indices)

import Data.Array
import Data.Complex
import Numeric.Transform.Fourier.FFT

-- Вспомогательные величины
fopt :: FileOptions
fopt = def{_fo_size=(600,200)}

-- Константы согласно заданию
n = 256

harmonics :: [Int]
harmonics = [0 .. floor (n/2)] 

function :: Double -> Double
function t = 5 * cos (3 * t) - 7 * sin (7 * t) + 4 * cos (11 * t)

maxFreq = 11 / (2 * pi)

k = [0.7, 1.0, 4]

lab3 :: IO () 
lab3 = do
  plotUndersampling
  plotUndersamplingAmpSpectrum
  plotUndersamplingPhaseSpectrum

  plotNormal
  plotNormalAmpSpectrum
  plotNormalPhaseSpectrum

  plotOversampling
  plotOversamplingAmpSpectrum
  plotOversamplingPhaseSpectrum


ampSpectrum signal = map magnitude $ elems $ rfft $ listArray (0, n-1) signal where
  n = length signal

phaseSpectrum signal = map phase $ elems $ rfft $ listArray (0, n-1) signal where
    n = length signal


-- Undersampling signal 
plotUndersampling =
  toFile fopt "posts/Lab3/undersamling_signal.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      discrets = [0.0, 1.0 /fd .. (n-1)/fd]
      signal = map function discrets
      fd = maxFreq * head k

      
plotUndersamplingAmpSpectrum = 
  toFile fopt "posts/Lab3/undersamling_spectrum.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = map (* (2 / n)) $ ampSpectrum $ map function discrets
      fd = maxFreq * head k
      harmonics = [0.0, 1.0 * cyclicFreq .. n / 2 * cyclicFreq]
      cyclicFreq =  2 * pi * fd / n 

plotUndersamplingPhaseSpectrum = 
  toFile fopt "posts/Lab3/undersamling_phase_spectrum.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = phaseSpectrum $ map function discrets
      fd = maxFreq * head k
      harmonics = [0.0, 1.0 * cyclicFreq .. n / 2 * cyclicFreq]
      cyclicFreq =  2 * pi * fd / n 

-- Normal signal 
plotNormal =
  toFile fopt "posts/Lab3/normal_signal.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      fd = maxFreq * k!!1

plotNormalAmpSpectrum = 
  toFile fopt "posts/Lab3/normal_spectrum.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = map (* (2 / n)) $ ampSpectrum $ map function discrets
      fd = maxFreq * k !! 1
      harmonics = [0.0, 1.0 * cyclicFreq .. n / 2 * cyclicFreq]
      cyclicFreq =  2 * pi * fd / n 
      
plotNormalPhaseSpectrum = 
  toFile fopt "posts/Lab3/normal_phase_spectrum.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = phaseSpectrum $ map function discrets
      fd = maxFreq * k !! 1
      harmonics = [0.0, 1.0 * cyclicFreq .. n / 2 * cyclicFreq]
      cyclicFreq =  2 * pi * fd / n 

-- Oversampling signal 
plotOversampling =
  toFile fopt "posts/Lab3/oversamling_signal.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      fd = maxFreq * k!!1

plotOversamplingAmpSpectrum = 
  toFile fopt "posts/Lab3/oversamling_spectrum.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = map (* (2 / n)) $ ampSpectrum $ map function discrets
      fd = maxFreq * k !! 2
      harmonics = [0.0, 1.0 * cyclicFreq .. n / 2 * cyclicFreq]
      cyclicFreq =  2 * pi * fd / n 

plotOversamplingPhaseSpectrum = 
  toFile fopt "posts/Lab3/oversamling_phase_spectrum.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = phaseSpectrum $ map function discrets
      fd = maxFreq * k !! 2
      harmonics = [0.0, 1.0 * cyclicFreq .. n / 2 * cyclicFreq]
      cyclicFreq =  2 * pi * fd / n 
    
