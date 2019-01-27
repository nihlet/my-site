module Lab3
  (lab3) where

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy             hiding (indices)

import Data.Array
import Data.Complex
import Numeric.Transform.Fourier.FFT

-- Вспомогательные величины
fopt :: FileOptions
fopt = def{_fo_size=(600,400)}

-- Константы согласно заданию
n = 1024

harmonics :: [Int]
harmonics = [0 .. floor (n/2)] 

function :: Double -> Double
function t = 5 * cos (3 * t) - 7 * sin (7 * t) + 4 * cos (11 * t)

-- todo: frequency and cyclic freq 
maxFreq = 22.0

k = [0.7, 1.0, 2.3]

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

-- signalFromHarmonics k signal = map ((*2) . realPart) $ elems $ ifft harmonics where
--   spectrum = rfft $ listArray (0, n-1) signal
--   harmonics = listArray (0, n-1) $ take k (elems spectrum) ++ replicate (n - k) (0.0 :+ 0.0)
--   n = length signal

-- calculateError s1 s2 = sqrt $ (/n) $ sum $ map (^^2) $ zipWith (-) s1 s2 where 
--   n = fromIntegral $ length s1

-- findMaxHarmonic01 :: [Double] -> Int
-- findMaxHarmonic01 = helper 0 where
--   helper h s1 = 
--     if calculateError s1 s2 < 0.1
--     then h
--     else helper (h+1) s1
--     where s2 = signalFromHarmonics h s1

-- Undersampling signal 
plotUndersampling =
  toFile fopt "posts/Lab3/undersamling_signal.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      fd = maxFreq * head k

plotUndersamplingAmpSpectrum = 
  toFile fopt "posts/Lab3/undersamling_spectrum.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = map (/n) $ ampSpectrum $ map function discrets
      fd = maxFreq * head k
      harmonics = [0.0, 1.0 / period .. (n-1) / period]
      period = n / fd

plotUndersamplingPhaseSpectrum = 
  toFile fopt "posts/Lab3/undersamling_phase_spectrum.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = map (/n) $ phaseSpectrum $ map function discrets
      fd = maxFreq * head k
      harmonics = [0.0, 1.0 / period .. (n-1) / period]
      period = n / fd


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
      spectrum = map (/n) $ ampSpectrum $ map function discrets
      fd = maxFreq * k !! 1
      harmonics = [0.0, 1.0 / period .. (n-1) / period]
      period = n / fd

plotNormalPhaseSpectrum = 
  toFile fopt "posts/Lab3/normal_phase_spectrum.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = map (/n) $ phaseSpectrum $ map function discrets
      fd = maxFreq * k !! 1
      harmonics = [0.0, 1.0 / period .. (n-1) / period]
      period = n / fd


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
      spectrum = map (/n) $ ampSpectrum $ map function discrets
      fd = maxFreq * k !! 2
      harmonics = [0.0, 1.0 / period .. (n-1) / period]
      period = n / fd

plotOversamplingPhaseSpectrum = 
  toFile fopt "posts/Lab3/oversamling_phase_spectrum.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = map (/n) $ phaseSpectrum $ map function discrets
      fd = maxFreq * k !! 2
      harmonics = [0.0, 1.0 / period .. (n-1) / period]
      period = n / fd

    
