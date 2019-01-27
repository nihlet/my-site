module Lab2
  (lab2) where

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy             hiding (indices)

import Data.Array
import Data.Complex
import Numeric.Transform.Fourier.FFT

-- Вспомогательные величины
fopt :: FileOptions
fopt = def{_fo_size=(600,400)}

-- Константы согласно заданию
f0 :: Double
f0 = 1400

k :: [Double]
k = [0.2, 1.6, 8.0]

m :: [Int]
m = [2, 5, 12]

n = 1024

harmonics :: [Int]
harmonics = [0 .. floor (n/2)] 

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

  plotOriginalIfft1
  plotOriginalIfft2
  plotOriginalIfft3
  plotOriginalIfftMax

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

signalFromHarmonics k signal = map ((*2) . realPart) $ elems $ ifft harmonics where
  spectrum = rfft $ listArray (0, n-1) signal
  harmonics = listArray (0, n-1) $ take k (elems spectrum) ++ replicate (n - k) (0.0 :+ 0.0)
  n = length signal

calculateError s1 s2 = sqrt $ (/n) $ sum $ map (^^2) $ zipWith (-) s1 s2 where 
  n = fromIntegral $ length s1

findMaxHarmonic01 :: [Double] -> Int
findMaxHarmonic01 = helper 0 where
  helper h s1 = 
    if calculateError s1 s2 < 0.1
    then h
    else helper (h+1) s1
    where s2 = signalFromHarmonics h s1

-- Original signal
plotOriginal =
  toFile fopt "posts/Lab2/Original_signal.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      fd = n / (periods * period)

plotOriginalAmpSpectrum = 
  toFile fopt "posts/Lab2/Original_spectrum.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = ampSpectrum $ map function discrets
      fd = n / (periods * period)

plotOriginalPhaseSpectrum = 
  toFile fopt "posts/Lab2/Original_phase_spectrum.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = phaseSpectrum $ map function discrets
      fd = n / (periods * period)

plotOriginalIfft1 = 
  toFile fopt "posts/Lab2/Original_signal_ifft_1.svg" $do
    setColors [opaque blue, opaque red]
    layout_title .= "D = " ++ show (calculateError signal signal2)
    plot (line "" [zip discrets signal])
    plot (line "M = 2" [zip discrets signal2])
    where
      fd = n / (periods * period)
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      signal2 = signalFromHarmonics h signal
      h = ceiling $ 2 * f0 / (fd / (n-1)) + 1
      

plotOriginalIfft2 = 
  toFile fopt "posts/Lab2/Original_signal_ifft_2.svg" $do
    setColors [opaque blue, opaque red]
    layout_title .= "D = " ++ show (calculateError signal signal2)
    plot (line "" [zip discrets signal])
    plot (line "M = 5" [zip discrets signal2]) where
      fd = n / (periods * period)
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      signal2 = signalFromHarmonics h $ map function discrets
      h = ceiling $ 5 * f0 / (fd / (n-1)) + 1

plotOriginalIfft3 = 
  toFile fopt "posts/Lab2/Original_signal_ifft_3.svg" $do
    setColors [opaque blue, opaque red]
    layout_title .= "D = " ++ show (calculateError signal signal2)
    plot (line "" [zip discrets signal])
    plot (line "M = 12" [zip discrets signal2]) where
      fd = n / (periods * period)
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      signal2 = signalFromHarmonics h $ map function discrets
      h = ceiling $ 12 * f0 / (fd / (n-1)) + 1

plotOriginalIfftMax = 
  toFile fopt "posts/Lab2/Original_signal_ifft_max.svg" $do
    setColors [opaque blue, opaque red]
    layout_title .= "D = " ++ show (calculateError signal signal2)
    plot (line "" [zip discrets signal])
    plot (line ("M = " ++ show (floor $ fromIntegral h / f0 * fd / (n-1))) [zip discrets signal2]) where
      fd = n / (periods * period)
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      signal2 = signalFromHarmonics h $ map function discrets
      h = findMaxHarmonic01 signal

-- Undersampling signal
plotUndersampling1 =
  toFile fopt "posts/Lab2/undersamling_signal1.svg" $do
    setColors [opaque blue]
    plot (line "" [zip discrets signal]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      fd = f0 * head k

plotUndersamplingAmpSpectrum1 = 
  toFile fopt "posts/Lab2/undersamling_spectrum1.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = ampSpectrum $ map function discrets
      fd = f0 * head k

plotUndersamplingPhaseSpectrum1 = 
  toFile fopt "posts/Lab2/undersamling_phase_spectrum1.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = phaseSpectrum $ map function discrets
      fd = f0 * head k

-- Undersampling signal 2
plotUndersampling2 =
  toFile fopt "posts/Lab2/undersamling_signal2.svg" $do
    setColors [opaque blue]
    plot (line "" [zip (take 32 discrets) signal]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      fd = f0 * k!!1

plotUndersamplingAmpSpectrum2 = 
  toFile fopt "posts/Lab2/undersamling_spectrum2.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = ampSpectrum $ map function discrets
      fd = f0 * k!!1

plotUndersamplingPhaseSpectrum2 = 
  toFile fopt "posts/Lab2/undersamling_phase_spectrum2.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = phaseSpectrum $ map function discrets
      fd = f0 * k!!1

-- Oversampling signal
plotOversampling =
  toFile fopt "posts/Lab2/oversamling_signal.svg" $do
    -- layout_title .= "Oversampling signal, k = 1.6"
    setColors [opaque blue]
    plot (line "" [zip (take 32 discrets) signal]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      signal = map function discrets
      fd = f0 * k!!2

plotOversamplingAmpSpectrum = 
  toFile fopt "posts/Lab2/oversamling_spectrum.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = ampSpectrum $ map function discrets
      fd = f0 * k!!2

plotOversamplingPhaseSpectrum = 
  toFile fopt "posts/Lab2/oversamling_phase_spectrum.svg" $do
    setColors [opaque blue]
    plot (line "" [zip harmonics spectrum]) where
      discrets = [0.0, 1.0/fd .. (n-1)/fd]
      spectrum = phaseSpectrum $ map function discrets
      fd = f0 * k!!2
    
